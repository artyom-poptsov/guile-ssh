/* Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of Guile-SSH
 *
 * Guile-SSH is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Guile-SSH is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __COMMON_H__
#define __COMMON_H__

#include <libguile.h>


/* Whether we're using Guile < 2.2.  */
#define USING_GUILE_BEFORE_2_2					\
  (SCM_MAJOR_VERSION < 2					\
   || (SCM_MAJOR_VERSION == 2 && SCM_MINOR_VERSION == 0))

/* Simplified version of 'SCM_DEFINE' macro that defines a procedure with
   empty docstring and without optional and "rest" arguments. */
#define SCM_GSSH_DEFINE(c_name, scheme_name, req, arglist) \
  SCM_DEFINE (c_name, scheme_name, req, 0, 0, arglist, "")


struct symbol_mapping {
  char* symbol;
  int   value;
};

extern SCM
_ssh_const_to_scm (const struct symbol_mapping *types, int value);

extern const struct symbol_mapping *
_scm_to_ssh_const (const struct symbol_mapping *types, SCM value);

extern SCM
_scm_object_hex_address (SCM obj);

#endif  /* ifndef __COMMON_H__ */

/* common.h ends here. */
