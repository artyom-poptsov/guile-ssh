## Config file for GNU Automake. 
##
## Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

ACLOCAL_AMFLAGS = -I m4 --install

SUBDIRS = am ssh examples build-aux m4 doc tests

EXTRA_DIST = \
	srfi/srfi-64.scm		\
	srfi/srfi-64.upstream.scm	\
	.dir-locals.el			\
	TODO

if HAVE_GUILE_VM
clean-go:
	@cd ssh; make clean-go
.PHONY: clean-go
endif HAVE_GUILE_VM

## Makefile.am ends here
