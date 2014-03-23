/* version.c -- Get information about versions.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

/* Get version of the libssh. */
SCM_DEFINE (guile_ssh_get_libssh_version, "get-libssh-version", 0, 0, 0,
            (),
            "Get version of the libssh.")
{
  return scm_from_locale_string (SSH_STRINGIFY (LIBSSH_VERSION));
}

/* Get version of the Guile-SSH. */
SCM_DEFINE (guile_ssh_get_library_version, "get-library-version", 0, 0, 0,
            (),
            "Get version of the Guile-SSH.")
{
  return scm_from_locale_string (PACKAGE_VERSION);
}


void
init_version (void)
{
#include "version.x"
}

/* version.c ends here */
