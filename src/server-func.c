/* server-func.c -- Functions for working with SSH server.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of libguile-ssh
 *
 * libguile-ssh is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * libguile-ssh is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/server.h>

#include "session-type.h"
#include "server-type.h"
#include "message-type.h"
#include "error.h"

/* Guile SSH specific options that are aimed to unificate the way of
   server configuration. */
enum gssh_server_options {
  /* Should not intersect with options from SSH server API. */
  GSSH_BIND_OPTIONS_BLOCKING_MODE = 100
};

/* SSH option mapping. */
struct option {
  char* symbol;
  int   type;
};


/* SSH server options mapping to Guile symbols. */
static struct option server_options[] = {
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
      return set_int32_opt (bind, type, value);

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
            "Set a SSH server option.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_server_set_x
{
  struct server_data *server_data = _scm_to_ssh_server (server);
  char *c_option_name;                    /* Name of an option */
  struct option *opt;                     /* Server option */
  int is_found = 0;
  int res;

  SCM_ASSERT (scm_is_symbol (option), option, SCM_ARG2, FUNC_NAME);

  c_option_name = scm_to_locale_string (scm_symbol_to_string (option));

  for (opt = server_options; opt->symbol != NULL; ++opt)
    {
      if (! strcmp (c_option_name, opt->symbol))
        {
          is_found = 1;
          break;
        }
    }

  if (! is_found)
    guile_ssh_error1 (FUNC_NAME, "No such option", option);

  res = set_option (server_data->bind, opt->type, value);
  if (res != SSH_OK)
    {
      guile_ssh_error1 (FUNC_NAME, "Unable to set the option",
                        scm_list_3 (server, option, value));
    }

  scm_remember_upto_here_1 (server);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_server_listen_x, "server-listen!", 1, 0, 0,
            (SCM server),
            "Start listening to the socket.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_server_listen_x
{
  struct server_data *server_data = _scm_to_ssh_server (server);
  int res = ssh_bind_listen (server_data->bind);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Couldn't listen the socket.", server);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_server_accept, "server-accept", 1, 0, 0,
            (SCM server),
            "Accept an incoming ssh connection to the server SERVER.\n"
            "Return a new SSH session.")
{
  struct server_data *server_data   = _scm_to_ssh_server (server);
  SCM session = guile_ssh_make_session ();
  struct session_data *session_data = _scm_to_ssh_session (session);
  int res = ssh_bind_accept (server_data->bind, session_data->ssh_session);
  return (res == SSH_OK) ? session : SCM_BOOL_F;
}


SCM_DEFINE (guile_ssh_server_handle_key_exchange,
            "server-handle-key-exchange", 1, 0, 0,
            (SCM session),
            "Handle key exchange for a server SERVER and setup encryption.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_server_handle_key_exchange
{
  struct session_data *session_data = _scm_to_ssh_session (session);
  int res = ssh_handle_key_exchange (session_data->ssh_session);
  if (res != SSH_OK)
    {
      guile_ssh_error1 (FUNC_NAME, ssh_get_error (session_data->ssh_session),
                        session);
    }
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_server_message_get,
            "server-message-get", 1, 0, 0,
            (SCM session),
            "Get a message.")
{
  SCM smob;
  struct session_data *session_data = _scm_to_ssh_session (session);
  struct message_data *message_data
    = (struct message_data *) scm_gc_malloc (sizeof (struct message_data),
                                             "message");

  message_data->message = ssh_message_get (session_data->ssh_session);
  if (! message_data->message)
    {
      scm_gc_free (message_data, sizeof (struct message_data), "message");
      return SCM_BOOL_F;
    }

  SCM_NEWSMOB (smob, message_tag, message_data);
  return smob;
}


/* Initialize server related functions. */
void
init_server_func (void)
{
#include "server-func.x"
}

/* server-func.c ends here */
