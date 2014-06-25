/* Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __SESSION_TYPE_H__
#define __SESSION_TYPE_H__

#include <libguile.h>
#include <libssh/libssh.h>
#include "channel-type.h"

extern scm_t_bits session_tag;


struct session_data {
  ssh_session ssh_session;

  /* The reason for storing references to all channels related to the
     session is that we have to prevent freeing of the channels that
     are normally freed along with the session. */
  size_t channel_cnt;
  struct channel_data **channels;
};


extern SCM guile_ssh_make_session (void);
extern SCM guile_ssh_is_session_p (SCM arg1);

extern void init_session_type (void);


/* Helper procedures */
extern struct session_data*_scm_to_session_data (SCM x);

#endif  /* ifndef __SESSION_TYPE_H__ */
