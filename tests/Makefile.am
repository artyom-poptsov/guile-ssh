## Config file for GNU Automake. 
##
## Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

SCM_TESTS = \
	server.scm \
	session.scm \
	client-server.scm \
	server-client.scm \
	sssh-ssshd.scm

TESTS = ${SCM_TESTS}

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = \
	abs_top_srcdir="$(abs_top_srcdir)"; export abs_top_srcdir; \
	ORIGTERM=${TERM}; export ORIGTERM; \
	TERM=xterm; export TERM; \
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE;

# LOG_COMPILER was introduced in Automake 1.12; don't expect "make
# check" or "make distcheck" to work with earlier versions.
SCM_LOG_COMPILER = \
	${top_builddir}/libtool \
	-dlopen ${top_builddir}/ssh/libguile-ssh.la \
	--mode=execute $(GUILE)

AM_SCM_LOG_FLAGS = \
	-L "$(top_srcdir)" \
	-s

EXTRA_DIST = \
	${SCM_TESTS}		\
	dsakey			\
	rsakey
