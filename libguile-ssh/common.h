/* Copyright (C) 2013-2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


/**
 * This macro is a little bit shorter than the original SCM_DEFINE macro, it
 * allows to define a Scheme procedure with N required parameters.
 */
#define SCM_DEFINE_N(c_name, scheme_name, req, arglist, docstring)      \
    SCM_DEFINE(c_name, scheme_name, req, 0, 0, arglist, docstring)

/**
 * Define a Scheme procedure with zero parameters.
 */
#define SCM_DEFINE_0(c_name, scheme_name, docstring)            \
    SCM_DEFINE(c_name, scheme_name, 0, 0, 0, (), docstring)

/**
 * Define a Scheme procedure with only one required parameter.
 */
#define SCM_DEFINE_1(c_name, scheme_name, arglist, docstring)           \
    SCM_DEFINE(c_name, scheme_name, 1, 0, 0, arglist, docstring)


/* The Guile-SSH port type.  Guile 2.2 introduced a new port API, so we have a
   separate implementation for these newer versions. */
#if USING_GUILE_BEFORE_2_2
typedef scm_t_bits       gssh_port_t;
#else
typedef scm_t_port_type* gssh_port_t;
#endif


struct gssh_symbol {
  char* symbol;
  int   value;
};

typedef struct gssh_symbol gssh_symbol_t;

extern SCM
gssh_symbol_to_scm (const gssh_symbol_t *types, int value);

extern const gssh_symbol_t *
gssh_symbol_from_scm (const gssh_symbol_t *types, SCM value);

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
