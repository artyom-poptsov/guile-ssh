## Copyright (C) 2013, 2014, 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-SSH.
##
## Guile-SSH is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-SSH is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/am/snarf

lib_LTLIBRARIES = libguile-ssh.la

libguile_ssh_la_SOURCES = \
	auth.c auth.h channel-func.c channel-func.h \
	channel-type.c channel-type.h channel-main.c \
	error.c error.h key-func.c key-func.h key-main.c \
	key-type.c key-type.h session-func.c session-func.h \
	session-type.c session-type.h session-main.c \
	server-main.c server-func.c server-func.h server-type.c \
	server-type.h message-type.c message-type.h message-func.c \
	message-func.h message-main.c \
	version.c threads.c threads.h \
	common.c common.h	\
	log.c log.h

BUILT_SOURCES = auth.x channel-func.x channel-type.x error.x \
	key-func.x key-type.x session-func.x session-type.x \
	server-type.x server-func.x message-type.x message-func.x \
	version.x log.x

libguile_ssh_la_CPPFLAGS = $(GUILE_CFLAGS)

libguile_ssh_la_LDFLAGS = -module -no-undefined -lssh_threads -lssh \
	-version-info $(LIBGUILE_SSH_INTERFACE) $(GUILE_LDFLAGS)

AM_CPPFLAGS = -I$(top_srcdir)/libguile-ssh -I$(top_builddir)/libguile-ssh

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(GUILE_CFLAGS) \
	$(AM_CPPFLAGS)

SUFFIXES = .x
.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x

## Makefile.am ends here