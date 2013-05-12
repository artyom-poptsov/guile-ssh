/* ssh-error.c -- Error reporting to Guile.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of libguile-ssh
 * 
 * LazyCat is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * LazyCat is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>

#include "ssh-error.h"

/* Report an error */
inline void
ssh_error (const char *subr, char *message, SCM args, SCM rest)
{
  SCM key = scm_from_locale_symbol (GUILE_SSH_EXCEPTION);
  scm_error (key, subr, message, args, rest);
}

/* ssh-error.c ends here */
