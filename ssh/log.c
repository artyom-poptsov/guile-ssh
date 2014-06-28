/* log.c -- Custom logging for Guile-SSH */

#include <libguile.h>
#include <libssh/libssh.h>

#include <sys/time.h>
#include <time.h>

#include "error.h"

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
  scm_display (function_name, scm_current_error_port ());
  scm_puts (": ", scm_current_error_port ());
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


/* Initialization */

void
init_log_func (void)
{
#include "log.x"
}

/* log.c ends here */
