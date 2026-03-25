# Guile-SSH -- An SSH library for GNU Guile.
#
# Process this file with autoconf to produce a configure script.
#
# Copyright © 2013-2024, 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
# Copyright © 2016-2017, 2019 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
# Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>

# -------------------------------------------------------------------------------
# Autoconf initialization.
# -------------------------------------------------------------------------------

AC_INIT([Guile-SSH],
  [1.0.0],
  [poptsov.artyom@gmail.com],
  [guile-ssh],
  [https://github.com/artyom-poptsov/guile-ssh])

# -------------------------------------------------------------------------------
dnl See
dnl <https://www.gnu.org/software/libtool/manual/html_node/Updating-version-info.html>
LIBGUILE_SSH_INTERFACE="19:0:1"
AC_SUBST(LIBGUILE_SSH_INTERFACE)

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux/m4])
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

AC_ARG_ENABLE([dsa],
  [AS_HELP_STRING([--enable-dsa],
  [Enable DSA support.])])

AS_IF([test "x$enable_dsa" = "xyes"],
  [AC_DEFINE(ENABLE_DSA, 1, [Enable DSA support.])],
  [AC_DEFINE(ENABLE_DSA, 0, [Disable DSA support.])])
AM_CONDITIONAL(ENABLE_DSA, $ENABLE_DSA)

# -------------------------------------------------------------------------------
# Check for the required libraries.
# -------------------------------------------------------------------------------

nl Checking for libssh 0.8.x.
PKG_CHECK_MODULES(
  [LIBSSH_0_8_3],
  [libssh >= 0.8.3],
  [AC_DEFINE(HAVE_LIBSSH_0_8_3, 1, [Use libssh >= 0.8.3])],
  [AC_DEFINE(HAVE_LIBSSH_0_8_3, 0, [Use libssh < 0.8.3])])

AM_CONDITIONAL(HAVE_LIBSSH_0_8_3, $HAVE_LIBSSH_0_8_3)

PKG_CHECK_MODULES([LIBSSH_0_9],
  [libssh >= 0.9.0],
  [AC_DEFINE(HAVE_LIBSSH_0_9, 1, [Use libssh 0.9])],
  [AC_DEFINE(HAVE_LIBSSH_0_9, 0, [Use libssh < 0.9])])

AM_CONDITIONAL(HAVE_LIBSSH_0_9, $HAVE_LIBSSH_0_9)

PKG_CHECK_MODULES([LIBSSH_0_10],
  [libssh >= 0.10.0],
  [AC_DEFINE(HAVE_LIBSSH_0_10, 1, [Use libssh 0.10])],
  [AC_DEFINE(HAVE_LIBSSH_0_10, 0, [Use libssh < 0.10])])

AM_CONDITIONAL(HAVE_LIBSSH_0_10, $HAVE_LIBSSH_0_10)

PKG_CHECK_MODULES([LIBSSH_0_11],
  [libssh >= 0.11.0],
  [AC_DEFINE(HAVE_LIBSSH_0_11, 1, [Use libssh 0.11])],
  [AC_DEFINE(HAVE_LIBSSH_0_11, 0, [Use libssh < 0.11])])

AM_CONDITIONAL(HAVE_LIBSSH_0_11, $HAVE_LIBSSH_0_11)

PKG_CHECK_MODULES([LIBSSH_0_12],
  [libssh >= 0.12.0],
  [AC_DEFINE(HAVE_LIBSSH_0_12, 1, [Use libssh 0.12])],
  [AC_DEFINE(HAVE_LIBSSH_0_12, 0, [Use libssh < 0.12])])

AM_CONDITIONAL(HAVE_LIBSSH_0_12, $HAVE_LIBSSH_0_12)

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

AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
  AC_MSG_ERROR(
    [`guile-snarf' not found.  Please install Guile 2.x, 3.x or later.])
fi

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

GUILE_EFFECTIVE_VERSION=`$GUILE -c '(display (effective-version))'`
AC_SUBST(GUILE_EFFECTIVE_VERSION)

AC_CONFIG_FILES([
  Makefile
  build-aux/Makefile
  build-aux/am/Makefile
  build-aux/m4/Makefile
  doc/Makefile
  examples/Makefile
  libguile-ssh/Makefile
  modules/Makefile
  modules/ssh/Makefile
  modules/ssh/dist/Makefile
  tests/Makefile
])

AM_CONDITIONAL([CROSS_COMPILING],
  [test "x$cross_compiling" = "xyes"])

if test "$guilesitedir" != "$GUILE_SITE"; then
  # Guile has a different prefix than this module
  AC_MSG_WARN([]
    [The Guile modules will be installed in ${guilesitedir}.]
    [You should probably re-run `configure' with]
    [`--with-guilesitedir=$GUILE_SITE']
    [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
    [variable.])
fi

# -------------------------------------------------------------------------------
# Check if DSA is enabled.
# -------------------------------------------------------------------------------

AS_IF(
  [test "x$enable_dsa" = "xyes"],
  [AC_MSG_WARN(
     []
     [Guile-SSH is configured with DSA public key algorithm support.]
     [Note that DSA support is disabled by default in libssh 0.10.]
     [<https://salsa.debian.org/debian/libssh/-/blob/debian/CHANGELOG#L37>]
     []
     [If your version of libssh does not support DSA public key algorithm]
     [it will lead to errors in Guile-SSH.])],
  [AC_MSG_NOTICE(
     [Guile-SSH configured without DSA public key algorithm support.])]
)

# -------------------------------------------------------------------------------

# Generate a Makefile, based on the results.
AC_OUTPUT

dnl configure.ac ends here.
