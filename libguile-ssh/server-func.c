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
#include "callbacks.h"

/* Guile SSH specific options that are aimed to unificate the way of
   server configuration. */
enum gssh_server_options {
  /* Should not intersect with options from SSH server API. */
  GSSH_BIND_OPTIONS_BLOCKING_MODE = 100,
  GSSH_BIND_OPTIONS_CALLBACKS
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
  { "callbacks",          GSSH_BIND_OPTIONS_CALLBACKS     },
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

static void
libssh_bind_incoming_connection_callback (ssh_bind bind, void* user_data)
{
  SCM server = (SCM) user_data;
  gssh_server_t* server_data = gssh_server_from_scm (server);
  SCM scm_callback = callback_ref (server_data->callbacks,
                                   "connect-status-callback");
  SCM scm_user_data = callback_userdata_ref (server_data->callbacks);
  printf("******************* aaaaaaaaaaaaaaaaa\n");
  scm_call_2 (scm_callback, server, scm_user_data);
}

/* Set libssh callbacks for a SERVER.  The procedure expects CALLBACKS to be
   an alist object.

   Return SSH_OK if callbacks were set succesfully, SSH_ERROR otherwise. */
static int
set_callbacks (SCM server, SCM callbacks)
{
  struct ssh_bind_callbacks_struct *cb
    = (struct ssh_bind_callbacks_struct *)
    scm_gc_malloc (sizeof (struct ssh_bind_callbacks_struct),
                   "ssh-bind-callbacks");

  gssh_server_t* server_data = gssh_server_from_scm (server);

  SCM_ASSERT (scm_to_bool (scm_list_p (callbacks)),
              callbacks,
              SCM_ARG3,
              "server-set!");

  server_data->callbacks = callbacks;

  if (callback_set_p (callbacks, "incoming-connection-callback"))
    {
      printf("******************* dfdf\n");
      callback_validate (server, callbacks, "incoming-connection-callback");
      cb->incoming_connection = libssh_bind_incoming_connection_callback;
    }

  ssh_callbacks_init (cb);
  printf("******************* %ld\n", cb->size);

  scm_remember_upto_here_2 (server, callbacks);

  return ssh_bind_set_callbacks (server_data->bind, cb, server);
}

static int
set_option (SCM server, ssh_bind bind, int type, SCM value)
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

    case GSSH_BIND_OPTIONS_CALLBACKS:
      return set_callbacks (server, value);

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

  res = set_option (server, server_data->bind, opt->value, value);

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


SCM_DEFINE (guile_ssh_server_accept, "server-accept", 1, 0, 0,
            (SCM server),
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
