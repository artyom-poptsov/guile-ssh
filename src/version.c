/* version.c -- Get information about versions.
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

/* Get version of the libssh. */
SCM
guile_ssh_get_libssh_version (void)
{
  return scm_from_locale_string (SSH_STRINGIFY (LIBSSH_VERSION));
}

/* Get version of the libguile-ssh. */
SCM
guile_ssh_get_library_version (void)
{
  return scm_from_locale_string (PACKAGE_VERSION);
}


void
init_version (void)
{
  scm_c_define_gsubr ("ssh:get-libssh-version", 0, 0, 0,
		      guile_ssh_get_libssh_version);
  scm_c_define_gsubr ("ssh:get-library-version", 0, 0, 0,
		      guile_ssh_get_library_version);
}

/* version.c ends here */
