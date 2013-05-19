/* channel-func.c -- SSH channel manipulation functions.
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
 * along with libguile-ssh.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>
#include <libssh/libssh.h>

#include "channel-type.h"


/* Functions */

/* Open a new session */
SCM
guile_ssh_channel_open_session (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  res = ssh_channel_open_session (data->ssh_channel);

  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Run a shell command CMD without an interactive shell. */
SCM
guile_ssh_channel_request_exec (SCM channel_smob, SCM cmd)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */
  char *c_cmd;			/* Command to execute. */

  SCM_ASSERT (scm_is_string (cmd), cmd, SCM_ARG2, __func__);

  c_cmd = scm_to_locale_string (cmd);

  res = ssh_channel_request_exec (data->ssh_channel, c_cmd);

  free (c_cmd);

  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Set an environment variable NAME to value VALUE */
SCM
guile_ssh_channel_request_env (SCM channel_smob, SCM name, SCM value)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  char *c_name;
  char *c_value;
  int res;			/* Result of a function call. */

  SCM_ASSERT (scm_is_string (name), name, SCM_ARG2, __func__);
  SCM_ASSERT (scm_is_string (value), value, SCM_ARG3, __func__);

  c_name  = scm_to_locale_string (name);
  c_value = scm_to_locale_string (value);

  res = ssh_channel_request_env (data->ssh_channel, c_name, c_value);

  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Poll a channel for data to read.  
 *
 * Return amount of data that can be read, or #f on error.
 */
SCM
guile_ssh_channel_pool (SCM channel_smob, SCM is_stderr)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  SCM_ASSERT (scm_is_bool (is_stderr), is_stderr, SCM_ARG2, __func__);

  res = ssh_channel_poll (data->ssh_channel, scm_is_true (is_stderr));
  
  if (res >= 0)
    return scm_from_int (res);
  else
    return SCM_BOOL_F;
}

/* Read data from the channel. */
SCM
guile_ssh_channel_read (SCM channel_smob, SCM count, SCM is_stderr)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */
  char *buffer;			/* Buffer for data. */
  uint32_t c_count;		/* Size of buffer. */
  SCM obtained_data;		/* Obtained data from the channel. */

  scm_dynwind_begin (0);

  SCM_ASSERT (scm_is_unsigned_integer (count, 0, UINT32_MAX), count,
	      SCM_ARG2, __func__);
  SCM_ASSERT (scm_is_bool (is_stderr), is_stderr, SCM_ARG3, __func__);

  c_count = scm_to_unsigned_integer (count, 0, UINT32_MAX);

  buffer = scm_gc_malloc (sizeof (char) * c_count, "data buffer");
  scm_dynwind_free (buffer);

  res = ssh_channel_read (data->ssh_channel, buffer, c_count, 
			  scm_is_true (is_stderr));

  if (res > 0)
    {
      buffer[c_count] = 0;	/* Avoid getting garbage in a SCM string */
      obtained_data = scm_from_locale_string (buffer);
    }
  else if (res == 0)
    {
      obtained_data = SCM_BOOL_F;
    }
  else
    {
      /* TODO: Improve error handling. */
      ssh_error (__func__, "Couldn't read data from a channel.",
		 SCM_BOOL_F, SCM_BOOL_F);
    }

  scm_dynwind_end ();

  return obtained_data;
}

/* Close a channel. */
SCM
guile_ssh_channel_close (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  res = ssh_channel_close (data->ssh_channel);

  return res ? SCM_BOOL_T : SCM_BOOL_F;
}


/* Predicates */

SCM
guile_ssh_channel_is_open_p (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  res = ssh_channel_is_open (data->ssh_channel);

  return res ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
guile_ssh_channel_is_eof_p (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  res = ssh_channel_is_eof (data->ssh_channel);
  
  return res ? SCM_BOOL_T : SCM_BOOL_F;
}


/* Initialize channel related functions. */
void
init_channel_func (void)
{
  scm_c_define_gsubr ("ssh:channel-open-session", 1, 0, 0,
		      guile_ssh_channel_open_session);
  scm_c_define_gsubr ("ssh:channel-request-exec", 2, 0, 0,
		      guile_ssh_channel_request_exec);
  scm_c_define_gsubr ("ssh:channel-request-env",  3, 0, 0,
		      guile_ssh_channel_request_env);
  
  scm_c_define_gsubr ("ssh:close-channel!", 1, 0, 0, guile_ssh_channel_close);  

  scm_c_define_gsubr ("ssh:channel-poll",   2, 0, 0, guile_ssh_channel_pool);
  scm_c_define_gsubr ("ssh:channel-read",   3, 0, 0, guile_ssh_channel_read);

  scm_c_define_gsubr ("ssh:channel-open?",  1, 0, 0, guile_ssh_channel_is_open_p);
  scm_c_define_gsubr ("ssh:channel-eof?",   1, 0, 0, guile_ssh_channel_is_eof_p);
}
