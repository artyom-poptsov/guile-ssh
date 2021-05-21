dnl   configuration script for Guile-SSH
dnl   Process this file with autoconf to produce configure.
dnl

define(GUILE_SSH_CONFIGURE_COPYRIGHT, [[
Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>

This file is part of Guile-SSH.

Guile-SSH is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Guile-SSH is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.
]])

AC_INIT([Guile-SSH], [0.13.1], [poptsov.artyom@gmail.com],
                     [guile-ssh],
                     [https://github.com/artyom-poptsov/guile-ssh])

AC_COPYRIGHT(GUILE_SSH_CONFIGURE_COPYRIGHT)

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADER([libguile-ssh/config.h])

AM_INIT_AUTOMAKE([color-tests])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])], [AC_SUBST([AM_DEFAULT_VERBOSITY],1)])

AC_PROG_CC
LT_INIT([disable-static])

if test "x$GCC" = "xyes"; then
  # Use compiler warnings.
  WARN_CFLAGS="-Wall"
else
  WARN_CFLAGS=""
fi
AC_SUBST([WARN_CFLAGS])

AC_ARG_WITH([guilesitedir],
            [AS_HELP_STRING([--with-guilesitedir],
                            [use the specified installation path for Guile modules])],
            [case "x$withval" in
             xyes|xno) guilesitedir="";;
             *)        guilesitedir="$withval";;
             esac],
             [guilesitedir=""])

# -------------------------------------------------------------------------------
# Check for needed libraries
# -------------------------------------------------------------------------------

PKG_CHECK_MODULES([LIBSSH], [libssh >= 0.6.4])
PKG_CHECK_MODULES([LIBSSH_0_7_3], [libssh >= 0.7.3],
                                [AC_DEFINE(HAVE_LIBSSH_0_7_3, 1, [Use libssh 0.7.3])],
                                [AC_WARN([
  Using a potentially vulnerable version of libssh
  <https://www.libssh.org/2016/02/23/libssh-0-7-3-security-and-bugfix-release/>])])

dnl Checking for libssh 0.8.x.
PKG_CHECK_MODULES([LIBSSH_0_8], [libssh >= 0.8.0],
                                [AC_DEFINE(HAVE_LIBSSH_0_8, 1, [Use libssh 0.8])],
                                [AC_DEFINE(HAVE_LIBSSH_0_8, 0, [Use libssh < 0.8])])

AM_CONDITIONAL(HAVE_LIBSSH_0_8, $HAVE_LIBSSH_0_8)

dnl NOTE that Ubuntu 18.04 LTS have "fake" libssh 0.8
dnl (0.8.0~20170825.94fa1e38-1ubuntu0.6) that is actually 0.7, so we need to
dnl check 0.8.1+ to make sure we have a valid libssh 0.8.
PKG_CHECK_MODULES([LIBSSH_0_8_1], [libssh >= 0.8.1],
                                  [AC_DEFINE(HAVE_LIBSSH_0_8_1, 1, [Use libssh 0.8.1])],
                                  [AC_DEFINE(HAVE_LIBSSH_0_8_1, 0, [Use libssh < 0.8.1])])

AM_CONDITIONAL(HAVE_LIBSSH_0_8_1, $HAVE_LIBSSH_0_8_1)

PKG_CHECK_MODULES([LIBSSH_0_8_3], [libssh >= 0.8.3],
                  [AC_DEFINE(HAVE_LIBSSH_0_8_3, 1, [Use libssh >= 0.8.3])],
                  [AC_DEFINE(HAVE_LIBSSH_0_8_3, 0, [Use libssh < 0.8.3])])
AM_CONDITIONAL(HAVE_LIBSSH_0_8_3, $HAVE_LIBSSH_0_8_3)

PKG_CHECK_MODULES([LIBSSH_0_9], [libssh >= 0.9.0],
                                [AC_DEFINE(HAVE_LIBSSH_0_9, 1, [Use libssh 0.9])],
                                [AC_DEFINE(HAVE_LIBSSH_0_9, 0, [Use libssh < 0.9])])

AM_CONDITIONAL(HAVE_LIBSSH_0_9, $HAVE_LIBSSH_0_9)

# -------------------------------------------------------------------------------

dnl These macros must be provided by guile.m4.
m4_pattern_forbid([^GUILE_PKG$])
m4_pattern_forbid([^GUILE_PROGS$])

dnl Use this macro so that 'GUILE_EFFECTIVE_VERSION' is defined here.
dnl Try Guile 3.0, then 2.2, and finally 2.0.
GUILE_PKG([3.0 2.2 2.0])

GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

if test "x$GUILD" = "x"; then
   GUILD=`which guild`
   AC_SUBST(GUILD)
fi

dnl (srfi srfi-64) appeared in Guile 2.0.10.  Make sure we have it.
GUILE_MODULE_AVAILABLE([have_srfi64], [(srfi srfi-64)])
if test "x$have_srfi64" != "xyes"; then
  AC_MSG_ERROR([(srfi srfi-64) is missing; please install a more recent Guile.])
fi

LT_INIT()

if test "x$guilesitedir" = "x"; then
   guilesitedir="$datadir/guile/site/$GUILE_EFFECTIVE_VERSION"
fi
AC_SUBST([guilesitedir])

LIBGUILE_SSH_INTERFACE="13:1:0"
AC_SUBST(LIBGUILE_SSH_INTERFACE)

GUILE_EFFECTIVE_VERSION=`$GUILE -c '(display (effective-version))'`
AC_SUBST(GUILE_EFFECTIVE_VERSION)

AC_CONFIG_FILES([Makefile libguile-ssh/Makefile examples/Makefile build-aux/Makefile])
AC_CONFIG_FILES([m4/Makefile doc/Makefile tests/Makefile am/Makefile])
AC_CONFIG_FILES([modules/Makefile modules/ssh/Makefile modules/ssh/dist/Makefile])

AM_CONDITIONAL([CROSS_COMPILING], [test "x$cross_compiling" = "xyes"])

# Generate a Makefile, based on the results.
AC_OUTPUT

if test "$guilesitedir" != "$GUILE_SITE"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in ${guilesitedir}.]
   [You should probably re-run `configure' with]
   [`--with-guilesitedir=$GUILE_SITE']
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi
