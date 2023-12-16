/* server-func.c -- Functions for working with SSH server.
 *
 * Copyright (C) 2013-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of Guile-SSH
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

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/server.h>

#include "common.h"
#include "session-type.h"
#include "session-func.h"
#include "server-type.h"
#include "message-type.h"
#include "error.h"
#include "log.h"
#include "key-type.h"
#include "callbacks.h"


const char* CALLBACK_SERVER_AUTH_PASSWORD = "server-auth-password-callback";
const char* CALLBACK_SERVER_AUTH_NONE     = "server-auth-none-callback";
const char* CALLBACK_SERVER_AUTH_PUBKEY   = "server-auth-pubkey-callback";


/* Guile SSH specific options that are aimed to unificate the way of
   server configuration. */
enum gssh_server_options {
  /* Should not intersect with options from SSH server API. */
  GSSH_BIND_OPTIONS_BLOCKING_MODE = 100
};


/* SSH server options mapping to Guile symbols. */
gssh_symbol_t server_options[] = {
  { "bindaddr",           SSH_BIND_OPTIONS_BINDADDR       },
  { "bindport",           SSH_BIND_OPTIONS_BINDPORT       },
  { "hostkey",            SSH_BIND_OPTIONS_HOSTKEY        },
  { "dsakey",             SSH_BIND_OPTIONS_DSAKEY         },
  { "rsakey",             SSH_BIND_OPTIONS_RSAKEY         },
  { "banner",             SSH_BIND_OPTIONS_BANNER         },
  { "log-verbosity",      SSH_BIND_OPTIONS_LOG_VERBOSITY  },
  { "blocking-mode",      GSSH_BIND_OPTIONS_BLOCKING_MODE },
  { NULL,                 -1                              }
};

/* Convert VALUE to a string and pass it to ssh_bind_options_set */
static inline int
set_string_opt (ssh_bind bind, int type, SCM value)
{
  char *str;
  int ret;

  SCM_ASSERT (scm_is_string (value),  value, SCM_ARG3, "server-set!");

  str = scm_to_locale_string (value);
  ret = ssh_bind_options_set (bind, type, str);
  free (str);

  return ret;
}

/* Convert VALUE to int32 and pass it to ssh_bind_options_set */
static inline int
set_int32_opt (ssh_bind bind, int type, SCM value)
{
  int32_t c_value;

  SCM_ASSERT (scm_is_integer (value), value, SCM_ARG3, "server-set!");

  c_value = scm_to_int (value);
  return ssh_bind_options_set (bind, type, &c_value);
}

/* Convert VALUE to uint32 and pass it to ssh_bind_options_set */
static inline int
set_uint32_opt (ssh_bind bind, int type, SCM value)
{
  unsigned int c_value;

  SCM_ASSERT (scm_is_unsigned_integer (value, 0, UINT32_MAX), value,
              SCM_ARG3, "server-set!");

  c_value = scm_to_uint32 (value);
  return ssh_bind_options_set (bind, type, &c_value);
}

/* Set a SSH bind BIND to blocking/nonblocking mode according to value
   VALUE.  VALUE is expected to be #t or #f.

   Always return SSH_OK. */
static inline int
set_blocking_mode (ssh_bind bind, SCM value)
{
  SCM_ASSERT (scm_is_bool (value), value, SCM_ARG2, "server-set!");
  ssh_bind_set_blocking (bind, scm_to_bool (value));
  return SSH_OK;
}

/* Convert Scheme symbol to libssh constant and set the corresponding
   option to the value of the constant. */
static inline int
set_sym_opt (ssh_bind bind, int type, gssh_symbol_t *sm, SCM value)
{
  const gssh_symbol_t *opt = gssh_symbol_from_scm (sm, value);
  if (! opt)
    guile_ssh_error1 ("server-set!", "Wrong value", value);
  return ssh_bind_options_set (bind, type, &opt->value);
}

static int
set_option (ssh_bind bind, int type, SCM value)
{
  switch (type)
    {
    case SSH_BIND_OPTIONS_BINDADDR:
    case SSH_BIND_OPTIONS_HOSTKEY:
    case SSH_BIND_OPTIONS_DSAKEY:
    case SSH_BIND_OPTIONS_RSAKEY:
    case SSH_BIND_OPTIONS_BANNER:
      return set_string_opt (bind, type, value);

    case SSH_BIND_OPTIONS_BINDPORT:
      return set_uint32_opt (bind, type, value);

    case SSH_BIND_OPTIONS_LOG_VERBOSITY:
      return set_sym_opt (bind, type, log_verbosity, value);

    case GSSH_BIND_OPTIONS_BLOCKING_MODE:
      return set_blocking_mode (bind, value);

    default:
      guile_ssh_error1 ("server-set!",
                        "Operation is not supported yet: %a~%",
                        scm_from_int (type));
    }

  return -1;                    /* ERROR */
}


SCM_DEFINE (guile_ssh_server_set_x, "server-set!", 3, 0, 0,
            (SCM server, SCM option, SCM value),
            "\
Set a SSH server option.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_server_set_x
{
  gssh_server_t *server_data = gssh_server_from_scm (server);
  const gssh_symbol_t *opt;		  /* Server option */
  int res;

  SCM_ASSERT (scm_is_symbol (option), option, SCM_ARG2, FUNC_NAME);

  opt = gssh_symbol_from_scm (server_options, option);

  if (! opt)
    guile_ssh_error1 (FUNC_NAME, "No such option", option);

  res = set_option (server_data->bind, opt->value, value);

  _gssh_log_debug_format(FUNC_NAME, scm_list_3 (server, option, value),
                         "result: %d", res);

  if (res != SSH_OK)
    {
      guile_ssh_error1 (FUNC_NAME, "Unable to set the option",
                        scm_list_3 (server, option, value));
    }

  server_data->options = scm_assoc_set_x (server_data->options, option, value);

  scm_remember_upto_here_1 (server);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_server_get, "server-get", 2, 0, 0,
            (SCM server, SCM option),
            "\
Get a Guile-SSH server option.  Return option value, or `#f' if option is\n\
not set.  Throw `guile-ssh-error' on error.\
")
#define FUNC_NAME s_guile_ssh_server_get
{
  const gssh_server_t *sd     = gssh_server_from_scm (server);
  const gssh_symbol_t *opt = gssh_symbol_from_scm (server_options, option);

  if (! opt)
    guile_ssh_error1 (FUNC_NAME, "No such option", option);

  return scm_assoc_ref (sd->options, option);
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_server_listen, "server-listen", 1, 0, 0,
            (SCM server),
            "\
Start listening to the socket.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_server_listen
{
  gssh_server_t *server_data = gssh_server_from_scm (server);
  int res = ssh_bind_listen (server_data->bind);

  _gssh_log_debug_format(FUNC_NAME, server, "result: %d", res);

  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Couldn't listen the socket.", server);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

static int
_server_auth_password_callback (ssh_session session,
                                const char* user,
                                const char* password,
                                void* userdata)
{
  SCM scm_list = (SCM) userdata;
  SCM scm_server = scm_list_ref (scm_list, scm_from_int (0));
  SCM scm_session = scm_list_ref (scm_list, scm_from_int (1));
  gssh_session_t *sd = gssh_session_from_scm (scm_session);

  SCM scm_callback = callback_ref (sd->callbacks,
                                   "server-auth-password-callback");
  if (scm_procedure_p (scm_callback))
    {
      SCM scm_userdata = callback_userdata_ref (sd->callbacks);
      SCM scm_user = scm_from_locale_string (user);
      SCM scm_password = scm_from_locale_string (password);
      SCM result = scm_call_5 (scm_callback,
                               scm_server,
                               scm_session,
                               scm_user,
                               scm_password,
                               scm_userdata);
      return scm_to_int (result);
    }
  else
    {
      return SSH_AUTH_DENIED;
    }
}

static int
_server_auth_none_callback (ssh_session session,
                            const char* user,
                            void* userdata)
{
  SCM scm_list = (SCM) userdata;
  SCM scm_server = scm_list_ref (scm_list, scm_from_int (0));
  SCM scm_session = scm_list_ref (scm_list, scm_from_int (1));
  gssh_session_t *sd = gssh_session_from_scm (scm_session);

  SCM scm_callback = callback_ref (sd->callbacks,
                                   "server-auth-none-callback");
  if (scm_procedure_p (scm_callback))
    {
      SCM scm_userdata = callback_userdata_ref (sd->callbacks);

      SCM result = scm_call_4 (scm_callback,
                               scm_server,
                               scm_session,
                               scm_from_locale_string (user),
                               scm_userdata);

      return scm_to_int (result);
    }
  else
    {
      return SSH_AUTH_DENIED;
    }
}

static int
_server_auth_pubkey_callback (ssh_session session,
                              const char* user,
                              ssh_key pubkey,
                              char signature_state,
                              void* userdata)
{
  SCM scm_list = (SCM) userdata;
  SCM scm_server = scm_list_ref (scm_list, scm_from_int (0));
  SCM scm_session = scm_list_ref (scm_list, scm_from_int (1));
  gssh_session_t *sd = gssh_session_from_scm (scm_session);

  SCM scm_callback = callback_ref (sd->callbacks,
                                   "server-auth-pubkey-callback");

  if (scm_procedure_p (scm_callback))
    {
      SCM scm_userdata = callback_userdata_ref (sd->callbacks);
      SCM scm_user = scm_from_locale_string (user);
      SCM scm_pubkey = gssh_key_to_scm (pubkey, session);
      SCM result = scm_call_6 (scm_callback,
                               scm_server,
                               scm_session,
                               scm_user,
                               scm_pubkey,
                               scm_from_int (signature_state),
                               scm_userdata);
      return scm_to_int (result);
    }
  else
    {
      return SSH_AUTH_DENIED;
    }
}


SCM_DEFINE (guile_ssh_server_accept, "%server-accept", 2, 0, 0,
            (SCM server, SCM callbacks),
            "\
Accept an incoming ssh connection to the SERVER.\n\
Throw `guile-ssh-error' on error.  Return a new SSH session.\
")
#define FUNC_NAME s_guile_ssh_server_accept
{
  gssh_server_t *server_data   = gssh_server_from_scm (server);
  SCM session = guile_ssh_make_session ();
  gssh_session_t *session_data = gssh_session_from_scm (session);
  int res = ssh_bind_accept (server_data->bind, session_data->ssh_session);

  _gssh_log_debug_format(FUNC_NAME, server, "result: %d", res);

  if (res != SSH_OK)
    guile_ssh_session_error1 (FUNC_NAME, session_data->ssh_session, session);

  if (! scm_is_false (callbacks))
    {
      SCM_ASSERT (scm_to_bool (scm_list_p (callbacks)),
                  callbacks,
                  SCM_ARG2,
                  "server-accept");

      session_data->callbacks = callbacks;

      struct ssh_server_callbacks_struct *cb;

      cb = (struct ssh_server_callbacks_struct *)
        scm_gc_malloc (sizeof (struct ssh_server_callbacks_struct),
                       "ssh-server-callbacks");

      cb->userdata = scm_list_2 (server, session);

      if (callback_set_p (callbacks, CALLBACK_SERVER_AUTH_PASSWORD))
        {
          callback_validate (session,
                             callbacks,
                             CALLBACK_SERVER_AUTH_PASSWORD);
          cb->auth_password_function = _server_auth_password_callback;
        }

      if (callback_set_p (callbacks, CALLBACK_SERVER_AUTH_NONE))
        {
          callback_validate (session,
                             callbacks,
                             CALLBACK_SERVER_AUTH_NONE);
          cb->auth_none_function = _server_auth_none_callback;
        }

      if (callback_set_p (callbacks, CALLBACK_SERVER_AUTH_PUBKEY))
        {
          callback_validate (session,
                             callbacks,
                            CALLBACK_SERVER_AUTH_PUBKEY);
          cb->auth_pubkey_function = _server_auth_pubkey_callback;
        }

      ssh_callbacks_init (cb);

      res = ssh_set_server_callbacks (session_data->ssh_session, cb);

      if (res != SSH_OK)
        {
          guile_ssh_error1 (FUNC_NAME,
                            "Could not set server callbacks",
                            server);
        }
    }

  return session;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_server_handle_key_exchange,
            "server-handle-key-exchange", 1, 0, 0,
            (SCM session),
            "\
Handle key exchange for a server SERVER and setup encryption.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_server_handle_key_exchange
{
  gssh_session_t *session_data = gssh_session_from_scm (session);
  int res = ssh_handle_key_exchange (session_data->ssh_session);

  _gssh_log_debug_format(FUNC_NAME, session, "result: %d", res);

  if (res != SSH_OK)
    guile_ssh_session_error1 (FUNC_NAME, session_data->ssh_session, session);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_server_message_get,
            "server-message-get", 1, 0, 0,
            (SCM session),
            "\
Get a message.\
")
{
  gssh_session_t *session_data = gssh_session_from_scm (session);
  ssh_message message = ssh_message_get (session_data->ssh_session);

  if (message == NULL)
    return SCM_BOOL_F;
  else
    return ssh_message_to_scm (message, session);
}


/* Initialize server related functions. */
void
init_server_func (void)
{
#include "server-func.x"
}

/* server-func.c ends here */
