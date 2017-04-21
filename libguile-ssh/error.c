/* ssh-error.c -- Error reporting to Guile.
 *
 * Copyright (C) 2013, 2014, 2015, 2016, 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of Guile-SSH.
 *
 * Guile-SSH is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Guile-SSH is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <libguile.h>
#include <libssh/libssh.h>

#include "error.h"

/* Report an error */
void
guile_ssh_error (const char *proc, const char *msg, SCM args, SCM rest)
{
  scm_error (scm_from_locale_symbol (GUILE_SSH_ERROR), proc, msg, args, rest);
}

/* Report an error (shorter version). */
void
guile_ssh_error1 (const char *proc, const char *msg, SCM args)
{
  char *c_str;
  scm_dynwind_begin (0);

  c_str = scm_to_locale_string (scm_object_to_string (args, SCM_UNDEFINED));
  scm_dynwind_free (c_str);

  _ssh_log (SSH_LOG_RARE, proc, "%s: %s", msg, c_str);

  scm_error (scm_from_locale_symbol (GUILE_SSH_ERROR), proc, msg, args,
             SCM_BOOL_F);

  scm_dynwind_end ();
}

/* Report a session error. */
void
guile_ssh_session_error1 (const char *proc, ssh_session session, SCM args)
{
  guile_ssh_error1 (proc, ssh_get_error (session), args);
}

/* ssh-error.c ends here */
