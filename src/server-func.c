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

/* SSH option mapping. */
struct option {
  char* symbol;
  int   type;
};


/* SSH server options mapping to Guile symbols. */

#define TYPE(OPT) SSH_BIND_OPTIONS_ ## OPT
static struct option server_options[] = {
  { "bindaddr",           TYPE (BINDADDR)      },
  { "bindport",           TYPE (BINDPORT)      },
  { "hostkey",            TYPE (HOSTKEY)       },
  { "dsakey",             TYPE (DSAKEY)        },
  { "rsakey",             TYPE (RSAKEY)        },
  { "banner",             TYPE (BANNER)        },
  { "log-verbosity",      TYPE (LOG_VERBOSITY) },
  { NULL,                 -1                   }
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

static int
set_option (ssh_bind bind, int type, SCM value)
{
  switch (type)
    {
    case TYPE (BINDADDR):
    case TYPE (HOSTKEY):
    case TYPE (DSAKEY):
    case TYPE (RSAKEY):
    case TYPE (BANNER):
      return set_string_opt (bind, type, value);

    case TYPE (BINDPORT):
      return set_uint32_opt (bind, type, value);

    case TYPE (LOG_VERBOSITY):
      return set_int32_opt (bind, type, value);

    default:
      guile_ssh_error1 ("server-set!",
                        "Operation is not supported yet: %a~%",
                        scm_from_int (type));
    }

  return -1;                    /* ERROR */
}

#undef TYPE


SCM_DEFINE (guile_ssh_server_set_x, "server-set!", 3, 0, 0,
            (SCM server, SCM option, SCM value),
            "Set a SSH server option.  Return #t on success, #f on error.")
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
    return SCM_BOOL_F;

  res = set_option (server_data->bind, opt->type, value);

  scm_remember_upto_here_1 (server);

  return (res == 0) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_server_listen, "server-listen", 1, 0, 0,
            (SCM server),
            "Start listening to the socket.\n"
            "Return #t on success, #f otherwise.")
{
  struct server_data *server_data = _scm_to_ssh_server (server);
  int res = ssh_bind_listen (server_data->bind);
  return (res == 0) ? SCM_BOOL_T : SCM_BOOL_F;
}


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
            "Return #t on success, #f otherwise.")
{
  struct session_data *session_data = _scm_to_ssh_session (session);
  int res = ssh_handle_key_exchange (session_data->ssh_session);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_DEFINE (guile_ssh_server_set_blocking_x, "server-set-blocking!", 2, 0, 0,
            (SCM server, SCM blocking),
            "Set the SERVER to blocking/nonblocking mode.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_server_set_blocking_x
{
  struct server_data *server_data = _scm_to_ssh_server (server);

  SCM_ASSERT (scm_is_bool (blocking), blocking, SCM_ARG2, FUNC_NAME);
  
  ssh_bind_set_blocking (server_data->bind, scm_to_bool (blocking));

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


static SCM scm_callback_proc = SCM_UNDEFINED;

static int
callback (ssh_session session, ssh_message message, void *data)
{
  scm_display (scm_from_locale_string ("callback called!\n"),
               scm_current_output_port ());
  if (! SCM_UNBNDP (scm_callback_proc))
    {
      SCM smob;
      struct session_data *session_data 
        = (struct session_data *) scm_gc_malloc (sizeof (struct session_data),
                                                 "session");
      session_data->ssh_session = session;
      SCM_NEWSMOB (smob, session_tag, session_data);
      /* scm_call_3 (scm_callback_proc, smob */
    }
  return 0;
}

SCM_DEFINE (guile_ssh_server_set_message_callback_x,
            "server-set-message-callback!", 3, 0, 0,
            (SCM session, SCM proc, SCM data),
            "Set callback procedure PROC for incoming messages for session "
            "SESSION.\n"
            "Return value is undefined.")
{
  struct session_data *session_data = _scm_to_ssh_session (session);
  ssh_set_message_callback (session_data->ssh_session, callback, NULL);
  return SCM_UNDEFINED;
}


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
