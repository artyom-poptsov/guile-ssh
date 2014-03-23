/* Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __CHANNEL_TYPE_H__
#define __CHANNEL_TYPE_H__

#include <libguile.h>
#include <libssh/libssh.h>

extern scm_t_bits channel_tag;


/* Smob data. */
struct channel_data {
  ssh_channel ssh_channel;
  uint8_t is_stderr;
};


/* API */

extern SCM guile_ssh_make_channel (SCM arg1);
extern SCM guile_ssh_is_channel_p (SCM arg1);

extern void init_channel_type (void);


/* Helper procedures */
extern struct channel_data *_scm_to_ssh_channel (SCM x);
extern SCM _ssh_channel_to_scm (ssh_channel ch);

#endif /* ifndef __CHANNEL_TYPE_H__ */
