/* message-type.c -- SSH message smob.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of libguile-ssh
 *
 * libguile-ssh is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * libguile-ssh is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/server.h>

#include "message-type.h"

scm_t_bits message_tag;         /* Smob tag. */


/* GC callbacks. */

SCM
mark_message (SCM message)
{
  return SCM_BOOL_F;
}

size_t
free_message (SCM message)
{
  struct message_data *msg_data = _scm_to_ssh_message (message);
  ssh_message_free (msg_data->message);
  return 0;
}


/* Predicates. */

SCM
equalp_message (SCM x1, SCM x2)
{
  struct message_data *msg1 = _scm_to_ssh_message (x1);
  struct message_data *msg2 = _scm_to_ssh_message (x2);

  if ((! msg1) || (! msg2))
    return SCM_BOOL_F;
  else if (msg1 != msg2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM_DEFINE (guile_ssh_is_message_p,
            "message?", 1, 0, 0,
            (SCM x),
            "Return #t if X a SSH message, #f otherwise.")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (message_tag, x));
}


/* Helper procedures. */

/* Convert X to a SSH message. */
struct message_data *
_scm_to_ssh_message (SCM x)
{
  scm_assert_smob_type (message_tag, x);
  return (struct message_data *) SCM_SMOB_DATA (x);
}


/* Message smob initialization. */
void
init_message_type (void)
{
  message_tag = scm_make_smob_type ("message", sizeof (struct message_data));
  scm_set_smob_mark (message_tag, mark_message);
  scm_set_smob_free (message_tag, free_message);
  scm_set_smob_equalp (message_tag, equalp_message);

#include "message-type.x"
}

/* message-type.c ends here */
