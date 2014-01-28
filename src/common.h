/* Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of libguile-ssh
 *
 * libguile-ssh is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * libguile-ssh is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __COMMON_H__
#define __COMMON_H__

#include <libguile.h>

struct symbol_mapping {
  char* symbol;
  int   value;
};

extern struct symbol_mapping log_verbosity[];

extern SCM
_ssh_const_to_scm (struct symbol_mapping *types, int value);

extern struct symbol_mapping *
_scm_to_ssh_const (struct symbol_mapping *types, SCM value);

#endif  /* ifndef __COMMON_H__ */

/* common.h ends here. */
