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
include $(top_srcdir)/am/guilec

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
	version.c base64.c base64.h threads.c threads.h \
	common.c common.h

BUILT_SOURCES = auth.x channel-func.x channel-type.x error.x \
	key-func.x key-type.x session-func.x session-type.x \
	server-type.x server-func.x message-type.x message-func.x \
	version.x

SCM_SOURCES = \
	auth.scm channel.scm key.scm session.scm	\
	server.scm message.scm version.scm

dist_noinst_HEADERS = $(BUILT_SOURCES)

libguile_ssh_la_CPPFLAGS = $(GUILE_CFLAGS)

libguile_ssh_la_LDFLAGS = -module -no-undefined -lssh_threads \
	-version-info $(LIBGUILE_SSH_INTERFACE) $(GUILE_LDFLAGS)

## Scheme files
pkgguilesitedir = $(guilesitedir)/ssh

nobase_dist_pkgguilesite_DATA = $(SCM_SOURCES)

ETAGS_ARGS = auth.scm channel.scm key.scm session.scm server.scm \
	message.scm version.scm

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(GUILE_CFLAGS)
SUFFIXES = .x
.c.x:
	$(AM_V_SNARF)$(GUILE_SNARF) -o $@ $< $(snarfcppopts)

CLEANFILES = *.x

if HAVE_GUILE_VM

GOBJECTS = $(SCM_SOURCES:%.scm=%.go)

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ssh
nobase_dist_ccache_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_ssh_install_go_files = install-nobase_dist_ccacheDATA
$(guile_ssh_install_go_files): install-nobase_dist_pkgguilesiteDATA

guilec_warnings =				\
  -Wunbound-variable -Warity-mismatch		\
  -Wunused-variable -Wunused-toplevel

guilec_opts = 					\
	--load-path=$(abs_srcdir)		\
	--load-path=$(abs_builddir)		\
	--target=$(host)			\
	$(guilec_warnings)

# TODO: Move environment setup to a separate file.
guilec_env  = 									\
	GUILE_AUTO_COMPILE=0 							\
	LD_LIBRARY_PATH="$(abs_top_builddir)/ssh/.libs/:${LD_LIBRARY_PATH}"	\
	GUILE_LOAD_PATH="$(abs_top_srcdir)"					\
	GUILE_LOAD_COMPILED_PATH="$(builddir)/ssh:$$GUILE_LOAD_COMPILED_PATH"

.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

CLEANFILES += $(GOBJECTS)

# Handy way to remove the .go files without removing all the rest.
clean-go:
	-$(RM) -f $(GOBJECTS)
.PHONY: clean-go

endif HAVE_GUILE_VM

## Makefile.am ends here
