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


/* Smob data. */
struct channel_data {
  /* Reference to the parent session.  We need to keep the reference
     to prevent the session from premature freeing by the GC. */
  SCM session;

  ssh_channel ssh_channel;
  uint8_t is_stderr;
};

/* Make sure that the channel data is valid.  Throw `guile-ssh-error' if the
   channel SCM has been closed and freed. */
#define GSSH_VALIDATE_CHANNEL_DATA(cd, scm, fn)                         \
  do {                                                                  \
    if (! cd)                                                           \
      guile_ssh_error1 (fn, "Channel has been closed and freed.", scm); \
  } while (0)

/* Make sure that the channel SCM is open. */
#define GSSH_VALIDATE_OPEN_CHANNEL(scm, pos, fn)                        \
  do {                                                                  \
    SCM_ASSERT_TYPE (SCM_OPPORTP (scm), scm, pos, fn, "open channel");  \
  } while (0)


/* API */

extern SCM guile_ssh_make_channel (SCM arg1, SCM flags);
extern SCM guile_ssh_is_channel_p (SCM arg1);
extern SCM guile_ssh_channel_get_session (SCM arg1);

extern void init_channel_type (void);


/* Helper procedures */
extern struct channel_data *_scm_to_channel_data (SCM x);
extern SCM _scm_from_channel_data (ssh_channel ch, SCM session,
                                   long flags);

#endif /* ifndef __CHANNEL_TYPE_H__ */
