/* channel-func.c -- SSH channel manipulation functions.
 *
 * Copyright (C) 2013, 2014, 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include "error.h"
#include "channel-type.h"


/* Procedures */

SCM_DEFINE (guile_ssh_channel_open_session, "channel-open-session", 1, 0, 0,
            (SCM channel),
            "\
Open a new session and mark the channel CHANNEL as opened port.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_channel_open_session
{
  struct channel_data *data = _scm_to_channel_data (channel);
  int res;
  GSSH_VALIDATE_CHANNEL_DATA (data, channel, FUNC_NAME);
  res = ssh_channel_open_session (data->ssh_channel);
  if (res != SSH_OK)
    {
      ssh_session session = ssh_channel_get_session (data->ssh_channel);
      guile_ssh_session_error1 (FUNC_NAME, session, channel);
    }

  SCM_SET_CELL_TYPE (channel, SCM_CELL_TYPE (channel) | SCM_OPN);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

/* Run a shell command CMD without an interactive shell. */
SCM_DEFINE (guile_ssh_channel_request_exec, "channel-request-exec", 2, 0, 0,
            (SCM channel, SCM cmd),
            "\
Run a shell command CMD without an interactive shell.\
")
#define FUNC_NAME s_guile_ssh_channel_request_exec
{
  struct channel_data *data = _scm_to_channel_data (channel);
  int res;
  char *c_cmd;                  /* Command to execute. */

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_string (cmd), cmd, SCM_ARG2, FUNC_NAME);

  c_cmd = scm_to_locale_string (cmd);
  res = ssh_channel_request_exec (data->ssh_channel, c_cmd);
  free (c_cmd);
  if (res != SSH_OK)
    {
      ssh_session session = ssh_channel_get_session (data->ssh_channel);
      guile_ssh_session_error1 (FUNC_NAME, session, scm_list_2 (channel, cmd));
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_get_exit_status,
            "channel-get-exit-status", 1, 0, 0,
            (SCM channel),
            "\
Get the exit status of the channel (error code from the executed \
instruction).  Return the exist status, or #f if no exit status has been \
returned (yet). \
")
#define FUNC_NAME s_guile_ssh_channel_get_exit_status
{
  struct channel_data *cd = _scm_to_channel_data (channel);
  int res;

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);

  res = ssh_channel_get_exit_status (cd->ssh_channel);
  return (res == SSH_ERROR) ? SCM_BOOL_F : scm_from_int (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_request_send_exit_status,
            "channel-request-send-exit-status", 2, 0, 0,
            (SCM channel, SCM exit_status),
            "\
Send the exit status to the remote process (as described in RFC 4254, section\n\
6.10).\
")
#define FUNC_NAME s_guile_ssh_channel_request_send_exit_status
{
  struct channel_data *cd = _scm_to_channel_data (channel);
  int res;

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);

  res = ssh_channel_request_send_exit_status (cd->ssh_channel,
                                              scm_to_uint32 (exit_status));
  if (res != SSH_OK)
    {
      ssh_session session = ssh_channel_get_session (cd->ssh_channel);
      guile_ssh_session_error1  (FUNC_NAME, session, channel);
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_request_pty, "channel-request-pty", 1, 0, 0,
            (SCM channel),
            "\
Request a PTY (pseudo terminal).\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_channel_request_pty
{
  struct channel_data *data = _scm_to_channel_data (channel);
  int res;

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);

  res = ssh_channel_request_pty (data->ssh_channel);
  if (res != SSH_OK)
    {
      ssh_session session = ssh_channel_get_session (data->ssh_channel);
      guile_ssh_session_error1  (FUNC_NAME, session, channel);
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_request_shell, "channel-request-shell", 1, 0, 0,
            (SCM channel),
            "\
Request a shell.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_channel_request_shell
{
  struct channel_data *data = _scm_to_channel_data (channel);
  int res;

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);

  res = ssh_channel_request_shell (data->ssh_channel);
  if (res != SSH_OK)
    {
      ssh_session session = ssh_channel_get_session (data->ssh_channel);
      guile_ssh_session_error1 (FUNC_NAME, session, channel);
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

/* Set an environment variable NAME to value VALUE 
   Return value is undefined. */
SCM_DEFINE (guile_ssh_channel_request_env, "channel-request-env", 3, 0, 0,
            (SCM channel, SCM name, SCM value),
            "\
Set an environment variable NAME to value VALUE.\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_channel_request_env
{
  struct channel_data *data = _scm_to_channel_data (channel);
  char *c_name;
  char *c_value;
  int res;

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_string (value), value, SCM_ARG3, FUNC_NAME);

  c_name  = scm_to_locale_string (name);
  c_value = scm_to_locale_string (value);
  res = ssh_channel_request_env (data->ssh_channel, c_name, c_value);
  if (res != SSH_OK)
    {
      ssh_session session = ssh_channel_get_session (data->ssh_channel);
      guile_ssh_session_error1 (FUNC_NAME, session, channel);
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_set_pty_size_x,
            "channel-set-pty-size!", 3, 0, 0,
            (SCM channel, SCM col, SCM row),
            "\
Change size of the PTY to columns COL and rows ROW.\n\
eturn value is undefined.\
")
#define FUNC_NAME s_guile_ssh_channel_set_pty_size_x
{
  struct channel_data *data = _scm_to_channel_data (channel);

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_unsigned_integer (col, 0, UINT32_MAX), col,
              SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_unsigned_integer (row, 0, UINT32_MAX), row,
              SCM_ARG2, FUNC_NAME);

  ssh_channel_change_pty_size (data->ssh_channel,
                               scm_to_uint32 (col),
                               scm_to_uint32 (row));

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_set_stream_x,
            "channel-set-stream!", 2, 0, 0,
            (SCM channel, SCM stream_name),
            "\
Set stream STREAM_NAME for channel CHANNEL.  STREAM_NAME must be one of the \n\
following symbols: \"stdout\" (default), \"stderr\".\n\
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_channel_set_stream_x
{
  struct channel_data *cd = _scm_to_channel_data (channel);

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_symbol (stream_name), stream_name, SCM_ARG2, FUNC_NAME);

  if (scm_is_eq (stream_name, scm_from_locale_symbol ("stdout")))
    {
      cd->is_stderr = 0;
    }
  else if (scm_is_eq (stream_name, scm_from_locale_symbol ("stderr")))
    {
      cd->is_stderr = 1;
    }
  else
    {
      guile_ssh_error1 (FUNC_NAME,
                        "Wrong stream name.  Possible names are: "
                        "'stdout, 'stderr", stream_name);
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_get_stream,
            "channel-get-stream", 1, 0, 0,
            (SCM channel),
            "\
Get current stream name from CHANNEL.  Throw `guile-ssh-error' on error.\n\
Return one of the following symbols: \"stdout\", \"stderr\".\
")
#define FUNC_NAME s_guile_ssh_channel_get_stream
{
  struct channel_data *cd = _scm_to_channel_data (channel);

  GSSH_VALIDATE_OPEN_CHANNEL (channel, SCM_ARG1, FUNC_NAME);

  if (cd->is_stderr == 0)
    return scm_from_locale_symbol ("stdout");
  if (cd->is_stderr == 1)
    return scm_from_locale_symbol ("stderr");

  guile_ssh_error1 (FUNC_NAME, "Wrong stream.",
                    scm_from_int (cd->is_stderr));
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_channel_get_session,
            "channel-get-session", 1, 0, 0,
            (SCM channel),
            "\
Get the session to which belongs the CHANNEL.  Throw `guile-ssh-error' on an \n\
error.  Return the session.\
")
#define FUNC_NAME s_guile_ssh_channel_get_session
{
  struct channel_data *cd = _scm_to_channel_data (channel);
  GSSH_VALIDATE_CHANNEL_DATA (cd, channel, FUNC_NAME);
  return cd->session;
}
#undef FUNC_NAME


/* Predicates */

SCM_DEFINE (guile_ssh_channel_is_open_p, "channel-open?", 1, 0, 0,
            (SCM channel),
            "Return #t if channel CHANNEL is open, #f otherwise.")
{
  struct channel_data *data = _scm_to_channel_data (channel);
  if (data && ssh_channel_is_open (data->ssh_channel))
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

SCM_DEFINE (guile_ssh_channel_is_eof_p, "channel-eof?", 1, 0, 0,
            (SCM channel),
            "\
Return #t if remote has set EOF, #f otherwise.\n\
Throw `guile-ssh-error' if the channel has been closed and freed.\
")
#define FUNC_NAME s_guile_ssh_channel_is_eof_p
{
  struct channel_data *data = _scm_to_channel_data (channel);
  GSSH_VALIDATE_CHANNEL_DATA (data, channel, FUNC_NAME);
  return scm_from_bool (ssh_channel_is_eof (data->ssh_channel));
}
#undef FUNC_NAME


/* Initialize channel related functions. */
void
init_channel_func (void)
{
#include "channel-func.x"
}

/* channel-func.c ends here */
