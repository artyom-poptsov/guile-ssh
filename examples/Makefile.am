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
## along with libguile-ssh.  If not, see
## <http://www.gnu.org/licenses/>.

EXTRA_DIST = \
	sssh.scm.in		\
	ssshd.scm.in		\
	echo/server.scm.in	\
	echo/client.scm.in	\
	rrepl.scm.in		\
	rpc/client.scm.in	\
	rpc/server.scm.in	\
	sscp.scm.in

examplesdir = $(pkgdatadir)/examples
examples_echodir = $(pkgdatadir)/examples/echo
examples_rpcdir = $(pkgdatadir)/examples/rpc
dist_examples_DATA = README sssh.scm ssshd.scm rrepl.scm sscp.scm
dist_examples_echo_DATA = echo/server.scm echo/client.scm
dist_examples_rpc_DATA = rpc/client.scm rpc/server.scm

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'

SUFFIXES = .in

.in:
	$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

CLEANFILES = \
	sssh.scm ssshd.scm			\
	echo/server.scm echo/client.scm		\
	rrepl.scm				\
	rpc/server.scm rpc/client.scm		\
	sscp.scm
