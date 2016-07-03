## Config file for GNU Automake. 
##
## Copyright (C) 2014, 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

include $(top_srcdir)/am/guilec

SCM_TESTS = \
	log.scm \
	server.scm \
	session.scm \
	client-server.scm \
	popen.scm \
	server-client.scm \
	sssh-ssshd.scm \
	key.scm \
	tunnel.scm \
	dist.scm

TESTS = ${SCM_TESTS}

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = \
	abs_top_srcdir="$(abs_top_srcdir)"; export abs_top_srcdir; \
	abs_top_builddir="$(abs_top_builddir)"; export abs_top_builddir; \
	ORIGTERM=${TERM}; export ORIGTERM; \
	TERM=xterm; export TERM; \
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE;

# LOG_COMPILER was introduced in Automake 1.12; don't expect "make
# check" or "make distcheck" to work with earlier versions.
SCM_LOG_COMPILER = \
	${top_builddir}/libtool \
	-dlopen ${top_builddir}/libguile-ssh/libguile-ssh.la \
	--mode=execute $(GUILE)

AM_SCM_LOG_FLAGS = \
	-L "$(top_srcdir)" \
	-L "$(top_srcdir)/modules"	\
	-s

EXTRA_DIST = \
	${SCM_TESTS}		\
	common.scm		\
	keys/dsakey		\
	keys/dsakey.pub		\
	keys/ecdsakey		\
	keys/ecdsakey.pub	\
	keys/rsakey		\
	keys/rsakey.pub		\
	config

### Compile modules

GOBJECTS = common.go

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ssh
nobase_dist_ccache_DATA = $(GOBJECTS)

guilec_warnings =				\
  -Wunbound-variable -Warity-mismatch		\
  -Wunused-variable -Wunused-toplevel

guilec_opts = 					\
	$(guilec_warnings)

# TODO: Move environment setup to a separate file.
guilec_env  = 									\
	GUILE_AUTO_COMPILE=0 							\
	LD_LIBRARY_PATH="$(abs_top_builddir)/libguile-ssh/.libs/:${LD_LIBRARY_PATH}"	\
	GUILE_LOAD_PATH="$(abs_top_srcdir)/modules"					\
	GUILE_LOAD_COMPILED_PATH="$(builddir)/ssh:$$GUILE_LOAD_COMPILED_PATH"

.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

###

CLEANFILES = \
	client-server-errors.log		\
	client-server-libssh.log		\
	dist-errors.log				\
	dist-libssh.log				\
	popen-errors.log			\
	popen-libssh.log			\
	server-client-errors.log		\
	server-client-libssh.log		\
	server-errors.log			\
	server-libssh.log			\
	session-errors.log			\
	session-libssh.log			\
	tunnel-errors.log			\
	tunnel-libssh.log			\
	$(GOBJECTS)
