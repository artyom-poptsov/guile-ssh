/* Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of Guile-SSH.
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

#ifndef __MESSAGE_TYPE_H__
#define __MESSAGE_TYPE_H__

#include <libguile.h>
#include <libssh/libssh.h>

extern scm_t_bits message_tag;


/* Smob data. */
struct message_data {
  ssh_message message;
};

extern void init_message_type (void);


/* Helper procedures. */
extern struct message_data *_scm_to_ssh_message (SCM x);

#endif  /* ifndef __MESSAGE_TYPE_H__ */

/* message-type.h ends here */
