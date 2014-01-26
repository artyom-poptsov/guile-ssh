## Config file for GNU Automake. 
##
## Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

ACLOCAL_AMFLAGS = -I m4 --install

SUBDIRS = src examples build-aux m4

# Tests.

SCM_TESTS = \
	tests/server.scm \
	tests/session.scm \
	tests/auth.scm \
	tests/client-server.scm

TESTS = SCM_TESTS

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_COMPILER = $(GUILE)
AM_SCM_LOG_FLAGS = -L "$(abs_top_srcdir)" --no-auto-compile

.PHONY: tests

tests: $(SCM_TESTS:%.scm=%.log)

CLEANFILES = tests/*.log
