## Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

SCM_SOURCES = \
	node.scm	\
	job.scm

EXTRA_DIST = \
	$(SCM_SOURCES)

distdir = $(guilesitedir)/ssh/dist

nobase_dist_dist_DATA = $(SCM_SOURCES)

GOBJECTS = $(SCM_SOURCES:%.scm=%.go)

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ssh/dist
nobase_dist_ccache_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_ssh_install_go_files = install-nobase_dist_ccacheDATA
$(guile_ssh_install_go_files): install-nobase_dist_distDATA

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

CLEANFILES = $(GOBJECTS)

# Handy way to remove the .go files without removing all the rest.
clean-go:
	-$(RM) -f $(GOBJECTS)
.PHONY: clean-go

## Makefile.am ends here
