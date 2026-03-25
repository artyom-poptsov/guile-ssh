## Copyright (C) 2013-2021, 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  agent.scm \
  auth.scm \
  channel.scm \
  dist.scm \
  key.scm \
  log.scm \
  message.scm \
  popen.scm \
  server.scm \
  session.scm \
  sftp.scm \
  shell.scm \
  tunnel.scm \
  version.scm

ETAGS_ARGS = $(SOURCES)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/ssh
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ssh

## Makefile.am ends here.
