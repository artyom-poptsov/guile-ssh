## Copyright (C) 2013-2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

include $(top_srcdir)/build-aux/am/guile.am

SUBDIRS = dist

SOURCES = \
	auth.scm channel.scm key.scm session.scm	\
	server.scm message.scm version.scm log.scm	\
	tunnel.scm dist.scm sftp.scm popen.scm		\
	shell.scm agent.scm

ETAGS_ARGS = auth.scm channel.scm key.scm session.scm server.scm \
	message.scm version.scm popen.scm agent.scm

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/ssh
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ssh

## Makefile.am ends here.
