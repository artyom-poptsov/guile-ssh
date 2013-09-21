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

#include "error.h"
#include "channel-type.h"


/* Functions */

/* Open a new session */
SCM_DEFINE (guile_ssh_channel_open_session, "channel-open-session", 1, 0, 0,
            (SCM channel),
            "Open a new session.")
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  int res = ssh_channel_open_session (data->ssh_channel);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Run a shell command CMD without an interactive shell. */
SCM_DEFINE (guile_ssh_channel_request_exec, "channel-request-exec", 2, 0, 0,
            (SCM channel, SCM cmd),
            "Run a shell command CMD without an interactive shell.")
#define FUNC_NAME s_guile_ssh_channel_request_exec
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  int res;
  char *c_cmd;                  /* Command to execute. */

  SCM_ASSERT (scm_is_string (cmd), cmd, SCM_ARG2, FUNC_NAME);

  c_cmd = scm_to_locale_string (cmd);
  res = ssh_channel_request_exec (data->ssh_channel, c_cmd);
  free (c_cmd);

  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

/* Set an environment variable NAME to value VALUE */
SCM_DEFINE (guile_ssh_channel_request_env, "channel-request-env", 3, 0, 0,
            (SCM channel, SCM name, SCM value),
            "Set an environment variable NAME to value VALUE")
#define FUNC_NAME s_guile_ssh_channel_request_env
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  char *c_name;
  char *c_value;
  int res;

  SCM_ASSERT (scm_is_string (name), name, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_string (value), value, SCM_ARG3, FUNC_NAME);

  c_name  = scm_to_locale_string (name);
  c_value = scm_to_locale_string (value);
  res = ssh_channel_request_env (data->ssh_channel, c_name, c_value);

  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

/* Poll a channel for data to read.
 *
 * Return amount of data that can be read, or #f on error.
 */
SCM_DEFINE (guile_ssh_channel_pool, "channel-poll", 2, 0, 0,
            (SCM channel, SCM is_stderr),
            "Poll a channel for data to read.\n"
            "Return amount of data that can be read, or #f on error.")
#define FUNC_NAME s_guile_ssh_channel_pool
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  int res;

  SCM_ASSERT (scm_is_bool (is_stderr), is_stderr, SCM_ARG2, FUNC_NAME);

  res = ssh_channel_poll (data->ssh_channel, scm_is_true (is_stderr));

  if (res >= 0)
    return scm_from_int (res);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Read data from the channel. */
SCM_DEFINE (guile_ssh_channel_read, "channel-read", 3, 0, 0,
            (SCM channel, SCM count, SCM is_stderr),
            "Read data from the channel CHANNEL.")
#define FUNC_NAME s_guile_ssh_channel_read
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  int res;
  char *buffer;                 /* Buffer for data. */
  uint32_t c_count;             /* Size of buffer. */
  SCM obtained_data = SCM_BOOL_F; /* Obtained data from the channel. */

  SCM_ASSERT (scm_is_unsigned_integer (count, 0, UINT32_MAX), count,
              SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_bool (is_stderr), is_stderr, SCM_ARG3, FUNC_NAME);

  c_count = scm_to_unsigned_integer (count, 0, UINT32_MAX);
  buffer = scm_gc_calloc (sizeof (char) * c_count + 1, "data buffer");
  res = ssh_channel_read (data->ssh_channel, buffer, c_count + 1,
                          scm_is_true (is_stderr));

  if (res > 0)
    {
      buffer[res] = 0;          /* Avoid getting garbage in a SCM string */
      obtained_data = scm_take_locale_string (buffer);
    }
  else if (res == 0)
    {
      obtained_data = SCM_BOOL_F;
    }
  else
    {
      /* TODO: Improve error handling. */
      guile_ssh_error1 (FUNC_NAME, "Couldn't read data from a channel.", 
                        SCM_BOOL_F);
    }

  return obtained_data;
}
#undef FUNC_NAME

/* Close a channel. */
SCM_DEFINE (guile_ssh_channel_close, "close-channel!", 1, 0, 0,
            (SCM arg1),
            "Close a channel.")
{
  struct channel_data *data = _scm_to_ssh_channel (arg1);
  int res = ssh_channel_close (data->ssh_channel);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_DEFINE (guile_ssh_channel_free, "free-channel!", 1, 0, 0,
            (SCM channel),
            "Free resourses allocated by channel CHANNEL")
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  ssh_channel_free (data->ssh_channel);
  return SCM_UNDEFINED;
}


/* Predicates */

SCM_DEFINE (guile_ssh_channel_is_open_p, "channel-open?", 1, 0, 0,
            (SCM channel),
            "Return #t if channel CHANNEL is open, #f otherwise.")
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  int res = ssh_channel_is_open (data->ssh_channel);
  return scm_from_bool (res);
}

SCM_DEFINE (guile_ssh_channel_is_eof_p, "channel-eof?", 1, 0, 0,
            (SCM channel),
            "Return #t if remote has set EOF, #f otherwise.")
{
  struct channel_data *data = _scm_to_ssh_channel (channel);
  int res = ssh_channel_is_eof (data->ssh_channel);
  return scm_from_bool (res);
}


/* Initialize channel related functions. */
void
init_channel_func (void)
{
#include "channel-func.x"
}

/* channel-func.c ends here */
