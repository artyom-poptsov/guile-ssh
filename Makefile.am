## Config file for GNU Automake. 
##
## Copyright (C) 2013, 2014, 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

ACLOCAL_AMFLAGS = -I build-aux/m4 --install

SUBDIRS = build-aux libguile-ssh examples doc modules tests

EXTRA_DIST =					\
	build-aux/config.rpath			\
	build-aux/compile			\
	.dir-locals.el				\
	TODO

gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    194fd7d..HEAD > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

dist-hook: gen-ChangeLog
.PHONY: gen-ChangeLog

clean-go:
	@cd modules; make clean-go
.PHONY: clean-go

## Makefile.am ends here
