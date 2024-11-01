/* session-func.c -- Functions for working with SSH session.
 *
 * Copyright (C) 2013-2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 * Copyright (C) 2024 Peter Tillemans <pti@snamellit.com>
 *
 * This file is part of Guile-SSH.
 *
 * Guile-SSH is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Guile-SSH is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>
#include <stdio.h>

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/callbacks.h>
#include <assert.h>

#include "common.h"
#include "error.h"
#include "session-type.h"
#include "key-type.h"
#include "message-type.h"
#include "log.h"
#include "callbacks.h"

/* Guile SSH specific options that are aimed to unificate the way of session
   configuration. */
enum gssh_session_options {
  /* Should not intersect with options from SSH session API. */
  GSSH_OPTIONS_CALLBACKS = 100
};


/* SSH options mapping to Guile symbols. */

static gssh_symbol_t session_options[] = {
  { "host",               SSH_OPTIONS_HOST               },
  { "port",               SSH_OPTIONS_PORT               },
  { "fd",                 SSH_OPTIONS_FD                 },
  { "bindaddr",           SSH_OPTIONS_BINDADDR           },
  { "user",               SSH_OPTIONS_USER               },
  { "ssh-dir",            SSH_OPTIONS_SSH_DIR            },
  { "identity",           SSH_OPTIONS_IDENTITY           },
  { "knownhosts",         SSH_OPTIONS_KNOWNHOSTS         },
  { "timeout",            SSH_OPTIONS_TIMEOUT            },
  { "timeout-usec",       SSH_OPTIONS_TIMEOUT_USEC       },
  { "ssh1",               SSH_OPTIONS_SSH1               },
  { "ssh2",               SSH_OPTIONS_SSH2               },
  { "log-verbosity",      SSH_OPTIONS_LOG_VERBOSITY      },
  { "ciphers-c-s",        SSH_OPTIONS_CIPHERS_C_S        },
  { "ciphers-s-c",        SSH_OPTIONS_CIPHERS_S_C        },
  { "compression-c-s",    SSH_OPTIONS_COMPRESSION_C_S    },
  { "compression-s-c",    SSH_OPTIONS_COMPRESSION_S_C    },
  { "proxycommand",       SSH_OPTIONS_PROXYCOMMAND       },
  { "stricthostkeycheck", SSH_OPTIONS_STRICTHOSTKEYCHECK },
  { "compression",        SSH_OPTIONS_COMPRESSION        },
  { "compression-level",  SSH_OPTIONS_COMPRESSION_LEVEL  },

#if HAVE_LIBSSH_0_8_1
  { "nodelay",            SSH_OPTIONS_NODELAY            },
#endif

#if HAVE_LIBSSH_0_8_3
  /* Preferred public key algorithms to be used for authentication
     (comma-separated list as a string). Example:
     "ssh-rsa,rsa-sha2-256,ssh-dss,ecdh-sha2-nistp256"

     This option was added to the libssh in
     4521ab73b6858efa0083ac96a1775719b1f649ae */
  { "public-key-accepted-types", SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES },
#endif

#if HAVE_LIBSSH_0_10
  {"rsa-min-size",        SSH_OPTIONS_RSA_MIN_SIZE       },
#endif

  { "callbacks",          GSSH_OPTIONS_CALLBACKS         },
  { NULL,                 -1 }
};


/* Create a new session. */
SCM_DEFINE (guile_ssh_make_session, "%make-session", 0, 0, 0,
            (),
            "\
Create a new session.\
")
{
  gssh_session_t *session_data = make_gssh_session ();

  session_data->ssh_session = ssh_new ();
  if (session_data->ssh_session == NULL)
    return SCM_BOOL_F;
  session_data->callbacks = SCM_BOOL_F;
  session_data->channels  = SCM_EOL;

  return gssh_session_to_scm (session_data);
}


/* Predicates */
SCM_DEFINE (guile_ssh_is_session_p, "session?", 1, 0, 0,
            (SCM x),
            "\
Return #t if X is a SSH session, #f otherwise.\
")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (session_tag, x));
}

/* Blocking flush of the outgoing buffer.

   Return on of the following symbols: 'ok, 'error. 'again.

   Asserts:
   - Return value of `ssh_blocking_flush' is one of the valid constants
     described in libssh.h */
SCM_DEFINE (guile_ssh_blocking_flush, "blocking-flush!", 2, 0, 0,
            (SCM session_smob, SCM timeout),
            "\
Blocking flush of the outgoing buffer.\n\
Return on of the following symbols: 'ok, 'error, 'again.\
")
#define FUNC_NAME s_guile_ssh_blocking_flush
{
  gssh_session_t *data = gssh_session_from_scm (session_smob);

  int c_timeout;                /* Timeout */
  int res;                      /* Result of a function call. */

  /* Check types */
  SCM_ASSERT (scm_is_integer (timeout), timeout, SCM_ARG2, FUNC_NAME);

  c_timeout = scm_to_int (timeout);

  res = ssh_blocking_flush (data->ssh_session, c_timeout);
  _gssh_log_debug_format (FUNC_NAME,
                          scm_list_2 (session_smob, timeout),
                          "result: %d",
                          res);
  switch (res)
    {
    case SSH_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_AGAIN:
      return scm_from_locale_symbol ("again");

    case SSH_ERROR:
      return scm_from_locale_symbol ("error");

    default:                    /* Must not happen. */
      _gssh_log_error_format (FUNC_NAME,
                              scm_list_2 (session_smob, timeout),
                              "Unknown result: %d",
                              res);
      assert (0);
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME


/* Set SSH session options */

/* Convert VALUE to a string and pass it to ssh_options_set */
static inline int
set_string_opt (ssh_session session, int type, SCM value)
{
  char *str;
  int ret;

  SCM_ASSERT (scm_is_string (value),  value, SCM_ARG3, "session-set!");

  str = scm_to_locale_string (value);
  ret = ssh_options_set (session, type, str);
  free (str);

  return ret;
}

/* Convert VALUE to uint64 and pass it to ssh_options_set */
static inline int
set_uint64_opt (ssh_session session, int type, SCM value)
{
  uint64_t c_value;

  SCM_ASSERT (scm_is_unsigned_integer (value, 0, UINT64_MAX), value,
              SCM_ARG3, "session-set!");

  c_value = scm_to_uint64 (value);
  return ssh_options_set (session, type, &c_value);
}

/* Convert VALUE to uint32 and pass it to ssh_options_set */
static inline int
set_uint32_opt (ssh_session session, int type, SCM value)
{
  unsigned int c_value;

  SCM_ASSERT (scm_is_unsigned_integer (value, 0, UINT32_MAX), value,
              SCM_ARG3, "session-set!");

  c_value = scm_to_uint32 (value);
  return ssh_options_set (session, type, &c_value);
}

/* Convert VALUE to int32 and pass it to ssh_options_set */
static inline int
set_int32_opt (ssh_session session, int type, SCM value)
{
  int32_t c_value;

  SCM_ASSERT (scm_is_integer (value), value, SCM_ARG3, "session-set!");

  c_value = scm_to_int (value);
  return ssh_options_set (session, type, &c_value);
}

/* Convert VALUE to integer that represents a boolan value (0
   considered as false, any other value is true), and pass it to
   ssh_options_set */
static inline int
set_bool_opt (ssh_session session, int type, SCM value)
{
  int32_t bool_value;

  SCM_ASSERT (scm_is_bool (value), value, SCM_ARG3, "session-set!");

  bool_value = scm_to_bool (value);
  return ssh_options_set (session, type, &bool_value);
}

/* Convert VALUE to a socket file descriptor and pass it to
   ssh_options_set */
static inline int
set_port_opt (ssh_session session, int type, SCM value)
{
  socket_t sfd;                 /* Socket File Descriptor */

  SCM_ASSERT (scm_port_p (value), value, SCM_ARG3, "session-set!");

  sfd = scm_to_int (scm_fileno (value));

  return ssh_options_set (session, type, &sfd);
}

/* Convert Scheme symbol to libssh constant and set the corresponding
   option to the value of the constant. */
static inline int
set_sym_opt (ssh_session session, int type, gssh_symbol_t *sm, SCM value)
{
  const gssh_symbol_t *opt = gssh_symbol_from_scm (sm, value);
  if (! opt)
    guile_ssh_error1 ("session-set!", "Wrong value", value);
  return ssh_options_set (session, type, &opt->value);
}


/* Callbacks. */

/* The callback procedure that meant to be called by libssh; the procedure in
   turn calls a specified Scheme procedure.  USERDATA is a Guile-SSH
   session instance. */
static void
libssh_global_request_callback (ssh_session session, ssh_message message,
                                void *userdata)
{
  SCM scm_session = (SCM) userdata;
  gssh_session_t *sd = gssh_session_from_scm (scm_session);

  SCM scm_callback = callback_ref (sd->callbacks, "global-request-callback");
  SCM scm_userdata = callback_userdata_ref (sd->callbacks);
  SCM scm_message = ssh_message_to_scm (message, scm_session);

  scm_call_3 (scm_callback, scm_session, scm_message, scm_userdata);
}

/* The callback procedure that meant to be called by libssh to indicate the
   percentage of connection steps completed.  The percentage is passed as
   STATUS.  USERDATA is a Guile-SSH session instance. */
static void
libssh_connect_status_callback (void *userdata, float status)
{
  SCM scm_session = (SCM) userdata;
  gssh_session_t *sd = gssh_session_from_scm (scm_session);

  SCM scm_callback = callback_ref (sd->callbacks, "connect-status-callback");
  SCM scm_userdata = callback_userdata_ref (sd->callbacks);

  scm_call_3 (scm_callback, scm_session, scm_from_double (status),
              scm_userdata);
}

/* Set libssh callbacks for a SESSION.  The procedure expects CALLBACKS to be
   an alist object.

   Return SSH_OK if callbacks were set succesfully, SSH_ERROR otherwise. */
static int
set_callbacks (SCM session, gssh_session_t *sd, SCM callbacks)
{
  struct ssh_callbacks_struct *cb
    = (struct ssh_callbacks_struct *)
    scm_gc_malloc (sizeof (struct ssh_callbacks_struct),
                   "ssh-callbacks");

  SCM_ASSERT (scm_to_bool (scm_list_p (callbacks)), callbacks, SCM_ARG3,
              "session-set!");

  sd->callbacks = callbacks;

  cb->userdata = session;

  if (callback_set_p (callbacks, "global-request-callback"))
    {
      callback_validate (session, callbacks, "global-request-callback");
      cb->global_request_function = libssh_global_request_callback;
    }

  if (callback_set_p (callbacks, "connect-status-callback"))
    {
      callback_validate (session, callbacks, "connect-status-callback");
      cb->connect_status_function = libssh_connect_status_callback;
    }

  ssh_callbacks_init (cb);

  scm_remember_upto_here_2 (session, callbacks);

  return ssh_set_callbacks (sd->ssh_session, cb);
}


/* Set an SSH session option. */
static int
set_option (SCM scm_session, gssh_session_t* sd, int type, SCM value)
{
  ssh_session session = sd->ssh_session;

  switch (type)
    {
    case SSH_OPTIONS_PORT:
      return set_uint32_opt (session, type, value);

    case SSH_OPTIONS_HOST:
    case SSH_OPTIONS_BINDADDR:
    case SSH_OPTIONS_USER:
    case SSH_OPTIONS_COMPRESSION:
    case SSH_OPTIONS_SSH_DIR:
    case SSH_OPTIONS_KNOWNHOSTS:
    case SSH_OPTIONS_IDENTITY:
    case SSH_OPTIONS_CIPHERS_C_S:
    case SSH_OPTIONS_CIPHERS_S_C:
    case SSH_OPTIONS_COMPRESSION_C_S:
    case SSH_OPTIONS_COMPRESSION_S_C:
    case SSH_OPTIONS_PROXYCOMMAND:
#if HAVE_LIBSSH_0_8_3
    case SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES:
#endif
      return set_string_opt (session, type, value);

    case SSH_OPTIONS_LOG_VERBOSITY:
      return set_sym_opt (session, type, log_verbosity, value);

    case SSH_OPTIONS_COMPRESSION_LEVEL:
      return set_int32_opt (session, type, value);

    case SSH_OPTIONS_TIMEOUT:
    case SSH_OPTIONS_TIMEOUT_USEC:
      return set_uint64_opt (session, type, value);

    case SSH_OPTIONS_SSH1:
    case SSH_OPTIONS_SSH2:
    case SSH_OPTIONS_STRICTHOSTKEYCHECK:
#if HAVE_LIBSSH_0_8_1
    case SSH_OPTIONS_NODELAY:
#endif
      return set_bool_opt (session, type, value);

    case SSH_OPTIONS_FD:
      return set_port_opt (session, type, value);

    case GSSH_OPTIONS_CALLBACKS:
      return set_callbacks (scm_session, sd, value);

#if ! HAVE_LIBSSH_0_8_3
    case SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES:
        guile_ssh_error1 ("session-set!",
                          "Option 'public-key-accepted-types' is not available"
                          " in the current version of libssh (%s)",
                          ssh_version (0));
        break;
#endif

#if HAVE_LIBSSH_0_10
    case SSH_OPTIONS_RSA_MIN_SIZE:
        return set_int32_opt (session, type, value);
        break;
#endif

    default:
      guile_ssh_error1 ("session-set!",
                        "Operation is not supported yet: %a~%",
                        scm_from_int (type));
    }

  return -1;                    /* ERROR */
}

/* Set a SSH option.  Return #t on success, #f on error. */
SCM_DEFINE (guile_ssh_session_set, "session-set!", 3, 0, 0,
            (SCM session, SCM option, SCM value),
            "\
Set a SSH option OPTION.  Throw an guile-ssh-error on error.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_session_set
{
  gssh_session_t* data = gssh_session_from_scm (session);
  const gssh_symbol_t *opt;	/* Session option */
  int res;                              /* Result of a function call */

  SCM_ASSERT (scm_is_symbol (option), option, SCM_ARG2, FUNC_NAME);

  opt = gssh_symbol_from_scm (session_options, option);

  if(! opt)
    guile_ssh_error1 (FUNC_NAME, "No such option", option);

  res = set_option (session, data, opt->value, value);

  _gssh_log_debug_format (FUNC_NAME, scm_list_3 (session, option, value),
                          "result: %d", res);

  if (res != SSH_OK) {
    guile_ssh_error1 (FUNC_NAME, "Unable to set the option",
                      scm_list_2 (option, value));
  }

  scm_remember_upto_here_1 (session);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


/* Options whose values can be requested through `session-get' */
static gssh_symbol_t session_options_getable[] = {
  { "host",         SSH_OPTIONS_HOST         },
  { "port",         SSH_OPTIONS_PORT         },
  { "user",         SSH_OPTIONS_USER         },
  { "identity",     SSH_OPTIONS_IDENTITY     },
  { "proxycommand", SSH_OPTIONS_PROXYCOMMAND },
  { "callbacks",    GSSH_OPTIONS_CALLBACKS   },
  { NULL,           -1                       }
};

SCM_DEFINE (guile_ssh_session_get, "session-get", 2, 0, 0,
            (SCM session, SCM option),
            "\
Get value of the OPTION.  Throw `guile-ssh-error' on an error.\
")
#define FUNC_NAME s_guile_ssh_session_get
{
  gssh_session_t* sd       = gssh_session_from_scm (session);
  const gssh_symbol_t *opt = NULL;
  SCM value = SCM_UNDEFINED;                    /*Value of the option */
  int res = SSH_OK;

  SCM_ASSERT (scm_is_symbol (option), option, SCM_ARG2, FUNC_NAME);

  opt = gssh_symbol_from_scm (session_options_getable, option);
  if (! opt)
    guile_ssh_error1 (FUNC_NAME, "Wrong option", option);

  if (opt->value == SSH_OPTIONS_PORT)
    {
      unsigned int port;
      res = ssh_options_get_port (sd->ssh_session, &port);
      value = (res == SSH_OK) ? scm_from_int (port) : SCM_UNDEFINED;
    }
  else if (opt->value == GSSH_OPTIONS_CALLBACKS)
    {
      value = sd->callbacks;
    }
  else
    {
      char *c_value = NULL;
      res = ssh_options_get (sd->ssh_session, opt->value, &c_value);

      _gssh_log_debug_format (FUNC_NAME, scm_list_2 (session, option),
                              "result: %d", res);

      value = (res == SSH_OK) ? scm_from_locale_string (c_value) : SCM_UNDEFINED;
    }

  if (res == SSH_ERROR)
    guile_ssh_error1 (FUNC_NAME, "Unable to get value of the option", option);

  scm_remember_upto_here_1 (option);

  return value;
}
#undef FUNC_NAME

/* Asserts:
   - SESSION is a Guile-SSH session object.
   - FILE_NAME either a string or '#f' */
SCM_GSSH_DEFINE (gssh_session_parse_config, "%gssh-session-parse-config!", 2,
                 (SCM session, SCM file_name))
#define FUNC_NAME s_gssh_session_parse_config
{
  gssh_session_t *sd = gssh_session_from_scm (session);
  int res;
  char* c_file_name = NULL;

  SCM_ASSERT (scm_is_string (file_name)
              || (scm_is_bool (file_name) && scm_is_false (file_name)),
              file_name, SCM_ARG2, FUNC_NAME);

  scm_dynwind_begin (0);

  if (scm_is_string (file_name))
    {
      c_file_name = scm_to_locale_string (file_name);
      scm_dynwind_free (c_file_name);
    }

  res = ssh_options_parse_config (sd->ssh_session,
                                  /* 'NULL' means that we should read the
                                     default '~/.ssh/config' file. */
                                  c_file_name);

  _gssh_log_debug_format (FUNC_NAME, scm_list_2 (session, file_name),
                          "result: %d", res);

  if (res != SSH_OK)
    {
      guile_ssh_error1 (FUNC_NAME, "Could not read the configuration file",
                        scm_list_2 (session, file_name));
    }

  scm_dynwind_end ();

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

/* Connect to the SSH server.

   Return one of the following symbols: 'ok, 'again, 'error

   Asserts:
   - Return value of `ssh_connect' is one of the valid constants described in
     libssh.h */
SCM_DEFINE (guile_ssh_connect_x, "connect!", 1, 0, 0,
            (SCM session),
            "\
Connect to the SSH server.\n\
Return one of the following symbols: 'ok, 'again, 'error\
")
#define FUNC_NAME s_guile_ssh_connect_x
{
  gssh_session_t* data = gssh_session_from_scm (session);
  int res = ssh_connect (data->ssh_session);
  _gssh_log_debug_format (FUNC_NAME, session, "result: %d", res);
  switch (res)
    {
    case SSH_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_AGAIN:
      return scm_from_locale_symbol ("again");

    case SSH_ERROR:
      return scm_from_locale_symbol ("error");

    default:                    /* Must not happen */
      assert (0);
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_disconnect, "disconnect!", 1, 0, 0,
            (SCM arg1),
            "\
Disconnect from a session (client or server).\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_disconnect
{
  gssh_session_t* session_data = gssh_session_from_scm (arg1);

  _gssh_log_debug (FUNC_NAME, "Disconnecting session ...",
                   arg1);
  if (ssh_is_connected (session_data->ssh_session))
    {
      _gssh_log_debug (FUNC_NAME, "Closing channels", arg1);
      gssh_session_close_all_channels_x (session_data);
      ssh_disconnect (session_data->ssh_session);
    }
  _gssh_log_debug (FUNC_NAME, "Disconnecting session ... done",
                   arg1);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_get_protocol_version, "get-protocol-version", 1, 0, 0,
            (SCM arg1),
            "\
Get SSH version.\n\
Return 1 for SSH1, 2 for SSH2 or #f on error.\
")
#define FUNC_NAME s_guile_ssh_get_protocol_version
{
  gssh_session_t* data = gssh_session_from_scm (arg1);
  SCM ret;
  int version;

  GSSH_VALIDATE_CONNECTED_SESSION (data, arg1, SCM_ARG1);

  version = ssh_get_version (data->ssh_session);

  if (version >= 0)
    ret = scm_from_int (version);
  else
    ret = SCM_BOOL_F;

  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_get_error, "get-error", 1, 0, 1,
            (SCM arg1),
            "\
Retrieve the error text message from the last error.\
")
{
  gssh_session_t* data = gssh_session_from_scm (arg1);
  SCM error = scm_from_locale_string (ssh_get_error (data->ssh_session));
  return error;
}

/* Authenticate the server.

   Return one of the following symbols: 'ok, 'known-changed,
   'found-other, 'not-known, 'file-not-found, 'error

   Asserts:
   - Return value of `ssh_is_server_known' is one of the valid constants
     described in libssh.h */
SCM_DEFINE (guile_ssh_authenticate_server, "authenticate-server", 1, 0, 0,
            (SCM session),
            "\
Authenticate the server.\n\
Return one of the following symbols: 'ok, 'known-changed, 'found-other,\n\
'not-known, 'file-not-found, 'error\
")
#define FUNC_NAME s_guile_ssh_authenticate_server
{
  gssh_session_t* data = gssh_session_from_scm (session);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (data, session, SCM_ARG1);

#if HAVE_LIBSSH_0_9
  res = ssh_session_is_known_server (data->ssh_session);
#else
  res = ssh_is_server_known (data->ssh_session);
#endif

  _gssh_log_debug_format (FUNC_NAME, session, "result: %d", res);

  switch (res)
    {
    case SSH_SERVER_KNOWN_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_SERVER_KNOWN_CHANGED:
      return scm_from_locale_symbol ("known-changed");

    case SSH_SERVER_FOUND_OTHER:
      return scm_from_locale_symbol ("found-other");

    case SSH_SERVER_NOT_KNOWN:
      return scm_from_locale_symbol ("not-known");

    case SSH_SERVER_FILE_NOT_FOUND:
      return scm_from_locale_symbol ("file-not-found");

    case SSH_SERVER_ERROR:
      return scm_from_locale_symbol ("error");

    default:                    /* Must not happen. */
      assert (0);
      return SCM_BOOL_F;
    }
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_get_server_public_key, "get-server-public-key", 1, 0, 0,
            (SCM session),
            "\
Get server public key from a SESSION.\n\
Return server's public key.  Throw `guile-ssh-error' on error.\
")
#define FUNC_NAME s_guile_ssh_get_server_public_key
{
  gssh_session_t *sd = gssh_session_from_scm (session);
  gssh_key_t *kd;
  int res;
  SCM key_smob;

  GSSH_VALIDATE_CONNECTED_SESSION (sd, session, SCM_ARG1);

  kd = make_gssh_key ();
  /* TODO: Check `kd' for NULL. */

#if HAVE_LIBSSH_0_8
  res = ssh_get_server_publickey (sd->ssh_session, &kd->ssh_key);
#else
  res = ssh_get_publickey (sd->ssh_session, &kd->ssh_key);
#endif

  _gssh_log_debug_format (FUNC_NAME, session, "result: %d", res);

  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Unable to get the server key", session);

  SCM_NEWSMOB (key_smob, key_tag, kd);

  return key_smob;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_write_known_host, "write-known-host!", 1, 0, 0,
            (SCM session),
            "\
Write the current server as known in the known hosts file.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_write_known_host
{
  gssh_session_t *session_data = gssh_session_from_scm (session);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (session_data, session, SCM_ARG1);

#if HAVE_LIBSSH_0_9
  res = ssh_session_update_known_hosts (session_data->ssh_session);
#else
  res = ssh_write_knownhost (session_data->ssh_session);
#endif

  _gssh_log_debug_format (FUNC_NAME, session, "result: %d", res);

  if (res != SSH_OK)
    guile_ssh_session_error1 (FUNC_NAME, session_data->ssh_session, session);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


/* Predicates */

SCM_DEFINE (guile_ssh_is_connected_p, "connected?", 1, 0, 0,
            (SCM arg1),
            "\
Check if we are connected.\n\
Return #f if we are connected to a server, #f if we aren't.\
")
#define FUNC_NAME s_guile_ssh_is_connected_p
{
  gssh_session_t* data = gssh_session_from_scm (arg1);
  int res = ssh_is_connected (data->ssh_session);
  _gssh_log_debug_format (FUNC_NAME, arg1, "result: %d", res);
  return scm_from_bool (res);
}
#undef FUNC_NAME


/* Initialize session related functions. */
void
init_session_func (void)
{
#include "session-func.x"
}

/* session-func.c ends here */
