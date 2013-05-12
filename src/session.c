/* session.c -- SSH session smob.
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
#include <string.h>

#include "session.h"
#include "ssh-error.h"

#define PRINT_DEBUG(data)\
  scm_display (data, scm_current_output_port ())

static scm_t_bits session_tag;	/* Smob tag. */

/* SSH options mapping to Guile symbols. */
struct option session_options[] = {
  { "host",              SSH_OPTIONS_HOST               },
  { "port",              SSH_OPTIONS_PORT               },
  { "port-str",          SSH_OPTIONS_PORT_STR           },
  { "fd",                SSH_OPTIONS_FD                 },
  { "user",              SSH_OPTIONS_USER               },
  { "ssh-dir",           SSH_OPTIONS_SSH_DIR            },
  { "identity",          SSH_OPTIONS_IDENTITY           },
  { "add-identity",      SSH_OPTIONS_ADD_IDENTITY       },
  { "knownhosts",        SSH_OPTIONS_KNOWNHOSTS         },
  { "timeout",           SSH_OPTIONS_TIMEOUT            },
  { "timeout-usec",      SSH_OPTIONS_TIMEOUT_USEC       },
  { "ssh1",              SSH_OPTIONS_SSH1               },
  { "ssh2",              SSH_OPTIONS_SSH2               },
  { "log-verbosity",     SSH_OPTIONS_LOG_VERBOSITY      },
  { "log-verbosity-str", SSH_OPTIONS_LOG_VERBOSITY_STR  },
  { "ciphers-c-s",       SSH_OPTIONS_CIPHERS_C_S        },
  { "ciphers-s-c",       SSH_OPTIONS_CIPHERS_S_C        },
  { "compression-c-s",   SSH_OPTIONS_COMPRESSION_C_S    },
  { "compression-s-c",   SSH_OPTIONS_COMPRESSION_S_C    },
  { "proxycommand",      SSH_OPTIONS_PROXYCOMMAND       },
  { "bindaddr",          SSH_OPTIONS_BINDADDR           },
  { "strcthostkeycheck", SSH_OPTIONS_STRICTHOSTKEYCHECK },
  { "compression",       SSH_OPTIONS_COMPRESSION        },
  { "compression-level", SSH_OPTIONS_COMPRESSION_LEVEL  },
  { NULL,                -1 }
};

SCM
mark_session (SCM session_smob)
{
  return SCM_BOOL_F;
}

/* Handle GC'ing of the session smob. */
size_t
free_session (SCM session_smob)
{
  struct session_data *data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  ssh_disconnect (data->ssh_session);
  ssh_free (data->ssh_session);
  return 0;
}

/* Blocking flush of the outgoing buffer. */
SCM
guile_ssh_blocking_flush (SCM session_smob, SCM timeout)
{
  struct session_data *data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  int c_timeout;		/* Timeout */
  int res;			/* Result of a function call. */

  SCM_ASSERT (scm_is_integer (timeout), timeout, SCM_ARG2, __func__);

  c_timeout = scm_to_int (timeout);

  res = ssh_blocking_flush (data->ssh_session, c_timeout);
  switch (res)
    {
    case SSH_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_ERROR:
      return scm_from_locale_symbol ("error");

    case SSH_AGAIN:
      return scm_from_locale_symbol ("again");
    }
}

/* Set an SSH session option. */
static void
set_option (ssh_session session, int type, SCM value)
{
#define OPTION_ASSERT(pred, value)			\
  do { SCM_ASSERT (pred,  value, SCM_ARG2, __func__); } while (0)

  int res = 0;			/* Result of an option setting */

  switch (type)
    {
    case SSH_OPTIONS_HOST:
      {
	char *host;

	OPTION_ASSERT (scm_is_string (value),  value);

	host = scm_to_locale_string (value);

	res = ssh_options_set (session, type, host);
	free (host);

	if (res < 0)
	  goto err;
      }
      return;

    case SSH_OPTIONS_PORT:
      {
	unsigned int port;

	OPTION_ASSERT (scm_is_unsigned_integer (value, 0, UINT32_MAX), value);

	port = scm_to_unsigned_integer (value, 0, UINT32_MAX);
	res = ssh_options_set (session, type, &port);
	if (res < 0)
	  goto err;
      }
      return;

    case SSH_OPTIONS_PORT_STR:
      {
	char *port_str = scm_to_locale_string (value);

	OPTION_ASSERT (scm_is_string (value), value);

	res = ssh_options_set (session, type, port_str);
	free (port_str);
	if (res < 0)
	  goto err;
      }
      return;

    case SSH_OPTIONS_USER:
      {
	char *user;

	OPTION_ASSERT (scm_is_string (value), value);

	user = scm_to_locale_string (value);
	res = ssh_options_set (session, type, user);
	free (user);
	if (res < 0)
	  goto err;
      }
      return;

    case SSH_OPTIONS_LOG_VERBOSITY:
      {
	int verbosity_level;

	OPTION_ASSERT (scm_is_integer (value), value);

	verbosity_level = scm_to_int (value);
	res = ssh_options_set (session, type, &verbosity_level);
	if (res < 0)
	  goto err;
      }
      return;

    case SSH_OPTIONS_LOG_VERBOSITY_STR:
      {
	char *verbosity_str;

	OPTION_ASSERT (scm_is_string (value), value);

	verbosity_str = scm_to_locale_string (value);
	res = ssh_options_set (session, type, verbosity_str);
	free (verbosity_str);
	if (res < 0)
	  goto err;
      }
      return;

      /* TODO: Implement all this. */

    case SSH_OPTIONS_FD:
    case SSH_OPTIONS_SSH_DIR:
    case SSH_OPTIONS_IDENTITY:
    case SSH_OPTIONS_ADD_IDENTITY:
    case SSH_OPTIONS_KNOWNHOSTS:
    case SSH_OPTIONS_TIMEOUT:
    case SSH_OPTIONS_TIMEOUT_USEC:
    case SSH_OPTIONS_SSH1:
    case SSH_OPTIONS_SSH2:
    case SSH_OPTIONS_CIPHERS_C_S:
    case SSH_OPTIONS_CIPHERS_S_C:
    case SSH_OPTIONS_COMPRESSION_C_S:
    case SSH_OPTIONS_COMPRESSION_S_C:
    case SSH_OPTIONS_PROXYCOMMAND:
    case SSH_OPTIONS_BINDADDR:
    case SSH_OPTIONS_STRICTHOSTKEYCHECK:
    case SSH_OPTIONS_COMPRESSION:
    case SSH_OPTIONS_COMPRESSION_LEVEL:
      goto notsupported;
    }

 err:
  ssh_error (__func__, "Couldn't set an option.",
	     SCM_BOOL_F, SCM_BOOL_F);
  return;

 notsupported:
  ssh_error (__func__, "Operation is not supported yet.",
	     SCM_BOOL_F, SCM_BOOL_F);

#undef OPTION_ASSERT
}

/* Set a SSH option. */
SCM
guile_ssh_session_set (SCM session_smob, SCM type, SCM value)
{
  struct session_data* data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  char *c_type_name;		        /* Name of an option */
  struct option *option;	        /* SSH option mapping */
  int is_found = 0;			/* Is a parameter found? */
  int res;				/* Result of a function call */

  if (scm_symbol_p (type) == SCM_BOOL_F)
    {
      ssh_error (__func__, "Wrong function call: "
		 "expected symbol as the second parameter",
		 SCM_BOOL_F, SCM_BOOL_F);
    }

  c_type_name = scm_to_locale_string (scm_symbol_to_string (type));

  for (option = session_options; option->symbol != NULL; ++option)
    {
      if (! strcmp (c_type_name, option->symbol))
	{
	  is_found = 1;
	  break;
	}
    }

  if(! is_found)
    {
      ssh_error (__func__, "Wrong option",
		 SCM_BOOL_F, SCM_BOOL_F);
    }

  /* FIXME: There is an error here. */
  scm_remember_upto_here_1 (session_smob);

  return SCM_UNDEFINED;
}

/* Get SSH version.
 *
 * Return 1 for SSH1, 2 for SSH2 or negative value on error.
 */
SCM
guile_ssh_get_version (SCM session_smob)
{
  struct session_data* data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  int version = ssh_get_version (data->ssh_session);

  return scm_from_int (version);
}

/* Create a new session. */
SCM
guile_ssh_make_session (void)
{
  SCM smob;

  struct session_data *session_data
    = (struct session_data *) scm_gc_malloc (sizeof (struct session_data),
					     "session");

  session_data->ssh_session = ssh_new ();
  if (session_data->ssh_session == NULL)
    {
      ssh_error (__func__, "Couldn't create a new SSH session.",
		 SCM_BOOL_F, SCM_BOOL_F);
    }

  SCM_NEWSMOB (smob, session_tag, session_data);

  return smob;
}

/* Connect to the SSH server. */
SCM
guile_ssh_connect (SCM session_smob)
{
  struct session_data* data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  int res = ssh_connect (data->ssh_session);

  switch (res)
    {
    case SSH_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_ERROR:
      return scm_from_locale_symbol ("error");

    case SSH_AGAIN:
      return scm_from_locale_symbol ("again");
    }
}

/* Disconnect from a session (client or server). */
SCM
guile_ssh_disconnect (SCM session_smob)
{
  struct session_data* data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);
  ssh_disconnect (data->ssh_session);
  return SCM_UNDEFINED;
}


/* Predicates */

/* Check if we are connected. */
SCM
guile_ssh_is_connected (SCM session_smob)
{
  struct session_data* data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  int res = ssh_is_connected (data->ssh_session);

  return res ? SCM_BOOL_T : SCM_BOOL_F;
}


/* session smob initialization. */
void
init_session_type (void)
{
  session_tag = scm_make_smob_type ("ssh:session", sizeof (struct session_data));
  scm_set_smob_mark (session_tag, mark_session);
  scm_set_smob_free (session_tag, free_session);

  scm_c_define_gsubr ("ssh:make-session",    0, 0, 0, guile_ssh_make_session);
  scm_c_define_gsubr ("ssh:blocking-flush!", 2, 0, 0, guile_ssh_blocking_flush);
  scm_c_define_gsubr ("ssh:session-set!",    3, 0, 0, guile_ssh_session_set);
  scm_c_define_gsubr ("ssh:get-version",     1, 0, 0, guile_ssh_get_version);
  scm_c_define_gsubr ("ssh:connect!",        1, 0, 0, guile_ssh_connect);
  scm_c_define_gsubr ("ssh:disconnect!",     1, 0, 0, guile_ssh_disconnect);    

  scm_c_define_gsubr ("ssh:connected?",      1, 0, 0, guile_ssh_is_connected);
}

/* session.c ends here */
