/* Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <stdio.h>
#include <libguile.h>

#include "error.h"


/* Predicate.  Return 1 if X is a Scheme procedure, 0 otherwise. */
static inline int
scm_is_procedure (SCM x)
{
    return scm_to_bool (scm_procedure_p (x));
}


/* Callbacks. */

/* Predicate.  Check if a callback NAME is present in CALLBACKS alist; return
   1 if it is, 0 otherwise. */
int
callback_set_p (SCM callbacks, const char* name)
{
    return scm_is_true (scm_assoc (scm_from_locale_symbol (name), callbacks));
}

/* Get an element NAME of the callbacks alist from a session data SD. */
SCM
callback_ref (SCM callbacks, const char* name)
{
    return scm_assoc_ref (callbacks, scm_from_locale_symbol (name));
}

/* Validate callback NAME.  Throw 'guile-ssh-error' exception on an error. */
void
callback_validate (SCM parent, SCM callbacks, const char* name)
{
    if (! scm_is_procedure (callback_ref (callbacks, name)))
        {
            enum { BUFSZ = 70 };
            char msg[BUFSZ];

            snprintf (msg, BUFSZ, "'%s' must be a procedure", name);

            guile_ssh_error1 ("callback_validate",
                              msg,
                              scm_list_2 (parent, callbacks));
        }
}


/* callbacks.c ends here. */
