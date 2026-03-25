## Copyright (C) 2013-2023, 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
## Copyright (C) 2026 Nicolas Graves <ngraves@ngraves.fr>
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

guileextensiondir = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions

guileextension_LTLIBRARIES = libguile-ssh.la

libguile_ssh_la_SOURCES = \
  auth.c \
  auth.h \
  callbacks.c \
  callbacks.h \
  channel-func.c \
  channel-func.h \
  channel-main.c \
  channel-type.c \
  channel-type.h \
  common.c \
  common.h \
  error.c \
  error.h \
  key-func.c \
  key-func.h \
  key-main.c \
  key-type.c \
  key-type.h \
  log.c \
  log.h \
  message-func.c \
  message-func.h \
  message-main.c \
  message-type.c \
  message-type.h \
  server-func.c \
  server-func.h \
  server-main.c \
  server-type.c \
  server-type.h \
  session-func.c \
  session-func.h \
  session-main.c \
  session-type.c \
  session-type.h \
  sftp-dir-func.c \
  sftp-dir-func.h \
  sftp-dir-main.c \
  sftp-dir-type.c \
  sftp-dir-type.h \
  sftp-file-main.c \
  sftp-file-type.c \
  sftp-file-type.h \
  sftp-session-func.c \
  sftp-session-func.h \
  sftp-session-main.c \
  sftp-session-type.c \
  sftp-session-type.h \
  threads.c \
  threads.h \
  version.c

BUILT_SOURCES = \
  auth.x \
  channel-func.x \
  channel-type.x \
  error.x \
  key-func.x \
  key-type.x \
  log.x \
  message-func.x \
  message-type.x \
  server-func.x \
  server-type.x \
  session-func.x \
  session-type.x \
  sftp-dir-func.x \
  sftp-dir-type.x \
  sftp-file-type.x \
  sftp-session-func.x \
  sftp-session-type.x \
  version.x

libguile_ssh_la_CPPFLAGS = $(CFLAGS) $(GUILE_CFLAGS)

SSH_LD_FLAGS = -lssh
if ! HAVE_LIBSSH_0_8_3
  SSH_LD_FLAGS += -lssh_threads
endif

libguile_ssh_la_LDFLAGS = \
  -module \
  -no-undefined $(SSH_LD_FLAGS) \
  -version-info $(LIBGUILE_SSH_INTERFACE) \
  $(GUILE_LDFLAGS)

AM_CFLAGS = $(WARN_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/libguile-ssh -I$(top_builddir)/libguile-ssh

snarfcppopts = \
  $(DEFS) \
  $(INCLUDES) \
  $(CPPFLAGS) \
  $(CFLAGS) \
  $(GUILE_CFLAGS) \
  $(AM_CPPFLAGS)

SUFFIXES = .x
.c.x:
	$(AM_V_GEN)CPP="$(CPP)" $(guile_snarf) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x

## Makefile.am ends here
