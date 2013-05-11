## Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of libguile-ssh.
## 
## libguile-ssh is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## libguile-ssh is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([libguile-ssh], [0.1], [poptsov.artyom@gmail.com])

AM_INIT_AUTOMAKE

AC_PROG_CC

GUILE_PROGS
GUILE_FLAGS

# Check for libssh
AC_SEARCH_LIBS([ssh_new], [ssh], [], AC_MSG_ERROR([libssh is not found.], [1]))

AC_CONFIG_FILES([Makefile])

# Generate a Makefile, based on the results.
AC_OUTPUT()
