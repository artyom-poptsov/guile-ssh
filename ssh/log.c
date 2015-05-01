/* log.c -- Guile-SSH logging procedures
 *
 * Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <libguile.h>
#include <libssh/libssh.h>

#include <sys/time.h>
#include <time.h>
#include <stdio.h>              /* DEBUG */
#include <unistd.h>             /* DEBUG */

#include "error.h"
#include "common.h"

/* Log verbosity levels used by libssh sessions and servers. */
struct symbol_mapping log_verbosity[] = {
  /* 0, No logging at all */
  { "nolog",              SSH_LOG_NOLOG     },
  /* 1, Only rare and noteworthy events */
  { "rare",               SSH_LOG_RARE      },
  /* 2, High level protocol information */
  { "protocol",           SSH_LOG_PROTOCOL  },
  /* 3, Lower level protocol infomations, packet level */
  { "packet",             SSH_LOG_PACKET    },
  /* 4, Every function path */
  { "functions",          SSH_LOG_FUNCTIONS },
  { NULL,                 -1                }
};

/* Whether the default calback was set or not. */
static int is_logging_callback_set = 0;

/* A Scheme log printer. */
static SCM logging_callback = SCM_BOOL_F;

/* The libssh logging callback which calls Scheme callback procedure. */
void
libssh_logging_callback (int        c_priority,
                         const char *c_function_name,
                         const char *c_message,
                         void       *c_userdata)
{
  SCM priority = scm_from_int (c_priority);
  SCM function = scm_from_locale_string (c_function_name);
  SCM message  = scm_from_locale_string (c_message);
  SCM userdata = (SCM) c_userdata;

  scm_call_4 (logging_callback, priority, function, message, userdata);
}


#define TBUF_SZ 64

static int
_get_current_timestring (char *buf, size_t len)
{
    char tbuf[TBUF_SZ];
    struct timeval tv;
    struct tm *tm;
    time_t t;

    gettimeofday (&tv, NULL);
    t = (time_t) tv.tv_sec;

    tm = localtime (&t);
    if (tm == NULL)
      return -1;

    strftime (tbuf, sizeof (tbuf) - 1, "%Y/%m/%d %H:%M:%S", tm);
    snprintf (buf, len, "%s.%06ld", tbuf, (long) tv.tv_usec);

    return 0;
}

SCM_DEFINE (guile_ssh_default_libssh_log_printer,
            "%default-libssh-log-printer", 4, 0, 0,
            (SCM priority, SCM function_name, SCM message, SCM user_data),
            "")
{
  char date[TBUF_SZ] = {0};
  int rc = _get_current_timestring (date, sizeof(date));

  scm_puts ("[", scm_current_error_port ());
  if (rc == 0)
    {
      scm_puts (date, scm_current_error_port ());
      scm_puts (", ", scm_current_error_port ());
    }

  scm_display (priority, scm_current_error_port ());
  scm_puts ("] ", scm_current_error_port ());
  scm_display (message, scm_current_error_port ());

  scm_newline (scm_current_error_port ());

  return SCM_UNDEFINED;
}

SCM_DEFINE (guile_ssh_set_logging_callback_x,
            "set-logging-callback!", 1, 0, 0,
            (SCM procedure),
            "")
#define FUNC_NAME s_guile_ssh_set_logging_callback_x
{
  SCM_ASSERT (scm_procedure_p (procedure), procedure, SCM_ARG1, FUNC_NAME);

  if (! is_logging_callback_set)
    {
      int res = ssh_set_log_userdata (SCM_BOOL_F);
      if (res != SSH_OK)
        guile_ssh_error1 (FUNC_NAME, "Could not set userdata", procedure);

      res = ssh_set_log_callback (&libssh_logging_callback);
      if (res != SSH_OK)
        guile_ssh_error1 (FUNC_NAME, "Could not setup logging", procedure);

      is_logging_callback_set = 1;
    }

  logging_callback = procedure;

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_get_logging_callback,
            "current-logging-callback", 0, 0, 0,
            (void),
            "")
{
  return logging_callback;
}

SCM_DEFINE (guile_ssh_set_log_useradata_x,
            "set-log-userdata!", 1, 0, 0,
            (SCM data),
            "")
#define FUNC_NAME s_guile_ssh_set_log_useradata_x
{
  int res = ssh_set_log_userdata (data);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Could not set userdata", data);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_get_log_userdata,
            "get-log-userdata", 0, 0, 0,
            (void),
            "")
{
  void *data = (void *) ssh_get_log_userdata ();
  return data ? (SCM) data : SCM_BOOL_F;
}


SCM_DEFINE (guile_ssh_write_log,
            "%write-log", 3, 0, 0,
            (SCM priority, SCM function_name, SCM message),
            "\
Write a MESSAGE to the libssh log with the given PRIORITY.  Return value is \n\
undefined. \
")
#define FUNC_NAME s_guile_ssh_write_log
{
  struct symbol_mapping *c_priority;
  char *c_function_name;
  char *c_message;

  SCM_ASSERT (scm_symbol_p (priority),      priority,      SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_string_p (function_name), function_name, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_string_p (message),       message,       SCM_ARG3, FUNC_NAME);

  c_priority = _scm_to_ssh_const (log_verbosity, priority);
  if (! c_priority)
    guile_ssh_error1 (FUNC_NAME, "Wrong priority level", priority);

  c_function_name = scm_to_locale_string (function_name);
  c_message       = scm_to_locale_string (message);

  _ssh_log (c_priority->value, c_function_name, "%s", c_message);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_set_log_verbosity_x,
            "set-log-verbosity!", 1, 0, 0,
            (SCM verbosity),
            "\
Set the global log verbosity to a VERBOSITY.  Throw `guile-ssh-error' on \
error.  Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_set_log_verbosity_x
{
  struct symbol_mapping *opt = _scm_to_ssh_const (log_verbosity, verbosity);
  int res;

  if (! opt)
    guile_ssh_error1 (FUNC_NAME, "Wrong verbosity level", verbosity);

  res = ssh_set_log_level (opt->value);
  if (res == SSH_ERROR)
    guile_ssh_error1 (FUNC_NAME, "Could not set log verbosity", verbosity);

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_get_log_verbosity,
            "get-log-verbosity", 0, 0, 0,
            (void),
            "\
Get global log verbosity value.\
")
{
  return _ssh_const_to_scm (log_verbosity, ssh_get_log_level ());
}


/* Initialization */

void
init_log_func (void)
{
#include "log.x"
}

/* log.c ends here */
