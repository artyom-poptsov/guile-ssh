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


struct gssh_session {
  ssh_session ssh_session;
  SCM callbacks;
  SCM channels;
};

typedef struct gssh_session gssh_session_t;

/* Make sure that the session pointed by session data structure pointer SD is
   connected. */
#define GSSH_VALIDATE_CONNECTED_SESSION(sd, scm, pos) \
  do { \
    SCM_ASSERT_TYPE (ssh_is_connected (sd->ssh_session), scm, \
                     pos, FUNC_NAME, "connected session"); \
  } while (0)


extern void gssh_session_add_channel_x (gssh_session_t* session, SCM channel);
extern void gssh_session_del_channel_x (gssh_session_t* session, SCM channel);
extern void gssh_session_close_all_channels_x (gssh_session_t* session);
extern gssh_session_t* make_gssh_session ();

extern void init_session_type (void);


/* Helper procedures */
extern SCM gssh_session_to_scm (gssh_session_t* session);
extern gssh_session_t* gssh_session_from_scm (SCM x);
extern int gssh_session_freed_p (SCM session);

#endif  /* ifndef __SESSION_TYPE_H__ */
