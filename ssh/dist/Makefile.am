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
