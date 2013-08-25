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
## along with libguile-ssh.  If not, see
## <http://www.gnu.org/licenses/>.

include $(top_srcdir)/am/snarf

AM_CFLAGS = @GUILE_CFLAGS@ `pkg-config --cflags guile-1.8` -Wall -g

lib_LTLIBRARIES = libguile-ssh.la

libguile_ssh_la_SOURCES = \
	auth.c auth.h channel-func.c channel-func.h \
	channel-type.c channel-type.h channel-main.c \
	error.c error.h key-func.c key-func.h key-main.c \
	key-type.c key-type.h session-func.c session-func.h \
	session-type.c session-type.h session-main.c \
	version.c base64.c threads.c threads.h

BUILT_SOURCES = auth.x channel-func.x channel-type.x error.x \
	key-func.x key-type.x session-func.x session-type.x \
	version.x

libguile_ssh_la_LDFLAGS = -module -no-undefined -lssh_threads \
	-version-info $(LIBGUILE_SSH_INTERFACE) $(GUILE_LDFLAGS)

## Scheme files
pkgguilesitedir = $(guilesitedir)/ssh

dist_pkgguilesite_DATA = auth.scm channel.scm key.scm session.scm	\
	version.scm

ETAGS_ARGS = auth.scm channel.scm key.scm session.scm version.scm

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
SUFFIXES = .x
.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x
