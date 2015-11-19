/* session-func.c -- Functions for working with SSH session.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

/* SSH option mapping. */
struct option {
  char* symbol;
  int   type;
};

/* Guile SSH specific options that are aimed to unificate the way of session
   configuration. */
enum gssh_session_options {
  /* Should not intersect with options from SSH session API. */
  GSSH_OPTIONS_CALLBACKS = 100
};


/* SSH options mapping to Guile symbols. */

static struct symbol_mapping session_options[] = {
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
  { "callbacks",          GSSH_OPTIONS_CALLBACKS         },
  { NULL,                 -1 }
};

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
  struct session_data *data = _scm_to_session_data (session_smob);

  int c_timeout;                /* Timeout */
  int res;                      /* Result of a function call. */

  /* Check types */
  SCM_ASSERT (scm_is_integer (timeout), timeout, SCM_ARG2, FUNC_NAME);

  c_timeout = scm_to_int (timeout);

  res = ssh_blocking_flush (data->ssh_session, c_timeout);
  switch (res)
    {
    case SSH_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_AGAIN:
      return scm_from_locale_symbol ("again");

    case SSH_ERROR:
      return scm_from_locale_symbol ("error");

    default:                    /* Must not happen. */
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
  int32_t bool;

  SCM_ASSERT (scm_is_bool (value), value, SCM_ARG3, "session-set!");

  bool = scm_to_bool (value);
  return ssh_options_set (session, type, &bool);
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
set_sym_opt (ssh_session session, int type, struct symbol_mapping *sm, SCM value)
{
  struct symbol_mapping *opt = _scm_to_ssh_const (sm, value);
  if (! opt)
    guile_ssh_error1 ("session-set!", "Wrong value", value);
  return ssh_options_set (session, type, &opt->value);
}


/* Callbacks. */

static void
libssh_global_request_callback (ssh_session session, ssh_message message,
                                void *userdata)
{
  SCM scm_session = (SCM) userdata;
  struct session_data *sd = _scm_to_session_data (scm_session);

  SCM scm_callback
    = scm_assoc_ref (sd->callbacks,
                     scm_from_locale_symbol ("global-request-callback"));

  SCM scm_userdata
    = scm_assoc_ref (sd->callbacks, scm_from_locale_symbol ("user-data"));

  SCM scm_message = _scm_from_ssh_message (message, scm_session);

  scm_call_3 (scm_callback, scm_session, scm_message, scm_userdata);
}

static int
set_callbacks (SCM session, struct session_data *sd, SCM callbacks)
{
  struct ssh_callbacks_struct *cb
    = (struct ssh_callbacks_struct *)
    scm_gc_malloc (sizeof (struct ssh_callbacks_struct),
                   "ssh-callbacks");

  SCM_ASSERT (scm_to_bool (scm_list_p (callbacks)), callbacks, SCM_ARG3,
              "session-set!");

  sd->callbacks = callbacks;

  cb->userdata = session;
  cb->global_request_function = libssh_global_request_callback;
  ssh_callbacks_init (cb);
  return ssh_set_callbacks (sd->ssh_session, cb);
}


/* Set an SSH session option. */
static int
set_option (SCM scm_session, struct session_data* sd, int type, SCM value)
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
      return set_bool_opt (session, type, value);

    case SSH_OPTIONS_FD:
      return set_port_opt (session, type, value);

    case GSSH_OPTIONS_CALLBACKS:
      return set_callbacks (scm_session, sd, value);

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
  struct session_data* data = _scm_to_session_data (session);
  struct symbol_mapping *opt;           /* Session option */
  int res;                              /* Result of a function call */

  SCM_ASSERT (scm_is_symbol (option), option, SCM_ARG2, FUNC_NAME);

  opt = _scm_to_ssh_const (session_options, option);

  if(! opt)
    guile_ssh_error1 (FUNC_NAME, "No such option", option);

  res = set_option (session, data, opt->value, value);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Unable to set the option", option);

  scm_remember_upto_here_1 (session);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


/* Options whose values can be requested through `session-get' */
static struct symbol_mapping session_options_getable[] = {
  { "host",         SSH_OPTIONS_HOST         },
  { "port",         SSH_OPTIONS_PORT         },
  { "user",         SSH_OPTIONS_USER         },
  { "identity",     SSH_OPTIONS_IDENTITY     },
  { "proxycommand", SSH_OPTIONS_PROXYCOMMAND },
  { NULL,           -1                       }
};

SCM_DEFINE (guile_ssh_session_get, "session-get", 2, 0, 0,
            (SCM session, SCM option),
            "\
Get value of the OPTION.  Throw `guile-ssh-error' on an error.\
")
#define FUNC_NAME s_guile_ssh_session_get
{
  struct session_data*sd     = _scm_to_session_data (session);
  struct symbol_mapping *opt = NULL;
  SCM value;                    /*Value of the option */
  int res;

  SCM_ASSERT (scm_is_symbol (option), option, SCM_ARG2, FUNC_NAME);

  opt = _scm_to_ssh_const (session_options_getable, option);
  if (! opt)
    guile_ssh_error1 (FUNC_NAME, "Wrong option", option);

  if (opt->value == SSH_OPTIONS_PORT)
    {
      unsigned int port;
      res = ssh_options_get_port (sd->ssh_session, &port);
      value = (res == SSH_OK) ? scm_from_int (port) : SCM_UNDEFINED;
    }
  else
    {
      char *c_value = NULL;
      res = ssh_options_get (sd->ssh_session, opt->value, &c_value);
      value = (res == SSH_OK) ? scm_from_locale_string (c_value) : SCM_UNDEFINED;
    }

  if (res == SSH_ERROR)
    guile_ssh_error1 (FUNC_NAME, "Unable to get value of the option", option);

  return value;
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
  struct session_data* data = _scm_to_session_data (session);
  int res = ssh_connect (data->ssh_session);
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
{
  struct session_data* session_data = _scm_to_session_data (arg1);
  ssh_disconnect (session_data->ssh_session);
  return SCM_UNDEFINED;
}

SCM_DEFINE (guile_ssh_get_protocol_version, "get-protocol-version", 1, 0, 0,
            (SCM arg1),
            "\
Get SSH version.\n\
Return 1 for SSH1, 2 for SSH2 or #f on error.\
")
#define FUNC_NAME s_guile_ssh_get_protocol_version
{
  struct session_data* data = _scm_to_session_data (arg1);
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
  struct session_data* data = _scm_to_session_data (arg1);
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
  struct session_data* data = _scm_to_session_data (session);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (data, session, SCM_ARG1);

  res = ssh_is_server_known (data->ssh_session);

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
  struct session_data *sd = _scm_to_session_data (session);
  struct key_data *kd;
  int res;
  SCM key_smob;

  GSSH_VALIDATE_CONNECTED_SESSION (sd, session, SCM_ARG1);

  kd = (struct key_data *) scm_gc_malloc (sizeof (struct key_data), "ssh key");
  /* TODO: Check `kd' for NULL. */

  res = ssh_get_publickey (sd->ssh_session, &kd->ssh_key);
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
  struct session_data *session_data = _scm_to_session_data (session);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (session_data, session, SCM_ARG1);

  res = ssh_write_knownhost (session_data->ssh_session);

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
{
  struct session_data* data = _scm_to_session_data (arg1);
  int res = ssh_is_connected (data->ssh_session);
  return scm_from_bool (res);
}


/* Initialize session related functions. */
void
init_session_func (void)
{
#include "session-func.x"
}

/* session-func.c ends here */
