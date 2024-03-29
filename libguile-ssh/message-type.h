/* Copyright (C) 2013-2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
struct gssh_message {
  /* Reference to the parent session.  We need to keep the reference
     to prevent the session from premature freeing by the GC. */
  SCM session;

  ssh_message message;
};

typedef struct gssh_message gssh_message_t;

extern void init_message_type (void);


/* Helper procedures. */
extern gssh_message_t* make_gssh_message ();
extern SCM ssh_message_to_scm (const ssh_message message, SCM session);
extern SCM gssh_message_to_scm (const gssh_message_t* message);
extern gssh_message_t* gssh_message_from_scm (SCM message);

#endif  /* ifndef __MESSAGE_TYPE_H__ */

/* message-type.h ends here */
