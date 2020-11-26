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


/* The Guile-SSH port type.  Guile 2.2 introduced a new port API, so we have a
   separate implementation for these newer versions. */
#if USING_GUILE_BEFORE_2_2
typedef scm_t_bits       gssh_port_t;
#else
typedef scm_t_port_type* gssh_port_t;
#endif


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


/* GC callbacks. */
typedef SCM    (*gc_mark_callback_t  )(SCM obj);
typedef size_t (*gc_free_callback_t  )(SCM obj);
typedef SCM    (*gc_equalp_callback_t)(SCM x1, SCM x2);
typedef int    (*gc_print_callback_t )(SCM obj, SCM port, scm_print_state* ps);

void set_smob_callbacks(scm_t_bits tag,
                        gc_mark_callback_t   mark_cb,
                        gc_free_callback_t   free_cb,
                        gc_equalp_callback_t equalp_cb,
                        gc_print_callback_t  print_cb);

#endif  /* ifndef __COMMON_H__ */

/* common.h ends here. */
