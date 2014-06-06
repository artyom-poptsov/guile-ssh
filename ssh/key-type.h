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

#ifndef __KEY_TYPE_H__
#define __KEY_TYPE_H__

#include <libguile.h>
#include <libssh/libssh.h>
#include "common.h"

extern scm_t_bits key_tag;

/* Smob data. */
struct key_data {
  ssh_key ssh_key;
};

extern struct symbol_mapping key_types[];


/* Procedures */

extern SCM guile_ssh_is_key_p (SCM arg1);
extern SCM guile_ssh_is_public_key_p (SCM arg1);
extern SCM guile_ssh_is_private_key_p (SCM arg1);

extern SCM guile_ssh_key_get_type (SCM arg1);

extern void init_key_type (void);


/* Helper procedures */
extern struct key_data *_scm_to_ssh_key (SCM x);
extern inline int _private_key_p (struct key_data *key);
extern inline int _public_key_p (struct key_data *key);

extern SCM _ssh_key_type_to_scm (int arg1);
extern struct symbol_mapping *_scm_to_ssh_key_type (SCM arg1);

#endif	/* ifndef __KEY_TYPE_H__ */
