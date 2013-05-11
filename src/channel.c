/* channel.c -- SSH channel smob.
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

#include "session.h"
#include "channel.h"
#include "ssh-error.h"

static scm_t_bits channel_tag;	/* Smob tag. */


/* Smob specific procedures */

SCM
mark_channel (SCM channel_smob)
{
  return SCM_BOOL_F;
}

size_t
free_channel (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  ssh_channel_free (data->ssh_channel);

  return 0;
}

/* Allocate a new SSH channel. */
SCM
guile_ssh_make_channel (SCM session_smob)
{
  SCM smob;

  struct session_data *session_data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  struct channel_data *channel_data 
    = (struct channel_data *) scm_gc_malloc (sizeof (struct channel_data),
					     "channel");
  channel_data->ssh_channel = ssh_channel_new (session_data->ssh_session);
  if (channel_data->ssh_channel == NULL)
    {
      ssh_error (__func__, "Couldn't allocate a new channel.",
		 SCM_BOOL_F, SCM_BOOL_F);
    }

  SCM_NEWSMOB (smob, channel_tag, channel_data);

  return smob;
}


/* SSH channel specific procedures. */

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

SCM
guile_ssh_channel_open_session (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  res = ssh_channel_open_session (data->ssh_channel);

  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Run a shell command without an interactive shell. */
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

  SCM_ASSERT (scm_is_boolean (is_stderr), is_stderr, SCM_ARG2, __func__);

  res = ssh_channel_poll (data->ssh_channel, scm_is_true (is_stderr));
  
  if (res >= 0)
    return scm_from_int (res);
  else
    return SCM_BOOL_F;
}

SCM
guile_ssh_channel_read (SCM channel_smob, SCM count, SCM is_stderr)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */
  char *buffer;			/* Buffer for data. */
  uint32_t c_count;		/* Size of buffer. */
  SCM data;			/* Obtained data from the channel. */

  scm_dynwind_begin (0);

  SCM_ASSERT (scm_is_unsigned_integer (count, 0, UINT32_MAX), count,
	      SCM_ARG2, __func__);
  SCM_ASSERT (scm_is_boolean (is_stderr), is_stderr, SCM_ARG3, __func__);

  c_count = scm_to_unsigned_integer (count, 0, UINT32_MAX);

  buffer = scm_gc_malloc (sizeof (char) * c_count, "data buffer");
  scm_dynwind_free (buffer);

  res = ssh_channel_read (data->ssh_channel, buffer, count, 
			  scm_is_true (is_stderr));

  if (res > 0)
    {
      data = scm_from_locale_string (buffer);
    }
  else if (res == 0)
    {
      data = SCM_BOOL_F;
    }
  else
    {
      ssh_error (__func__, "Couldn't read data from a channel.",
		 SCM_BOOL_F, SCM_BOOL_F);
    }

  scm_dynwind_end ();

  return data;
}


/* Predicates */

SCM
guile_ssh_channel_is_open (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  res = ssh_channel_is_open (data->ssh_channel);

  return res ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
guile_ssh_channel_is_eof (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  int res; 			/* Result of a function call. */

  res = ssh_channel_is_eof (data->ssh_channel);
  
  return res ? SCM_BOOL_T : SCM_BOOL_F;
}


/* channel smob initialization. */
void
init_channel_type (void)
{
  channel_tag = scm_make_smob_type ("ssh:channel", sizeof (struct channel_data));
  scm_set_smob_mark (channel_tag, mark_channel);
  scm_set_smob_free (channel_tag, free_channel); 

  scm_c_define_gsubr ("ssh:make-channel",   1, 0, 0, guile_ssh_make_channel);
  scm_c_define_gsubr ("ssh:close-channel!", 1, 0, 0, guile_ssh_channel_close);
  scm_c_define_gsubr ("ssh:channel-request-exec", 2, 0, 0,
		      guile_ssh_channel_request_exec);

  scm_c_define_gsubr ("ssh:channel-poll",   2, 0, 0, guile_ssh_channel_pool);
  scm_c_define_gsubr ("ssh:channel-read",   3, 0, 0, guile_ssh_channel_read);

  scm_c_define_gsubr ("ssh:channel-open?",  1, 0, 0, guile_ssh_channel_is_open);
  scm_c_define_gsubr ("ssh:channel-eof?",   1, 0, 0, guile_ssh_channel_is_eof);
}

/* channel.c ends here */
