/* message-type.c -- SSH message smob.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/server.h>

#include "message-type.h"
#include "message-func.h"
#include "common.h"

scm_t_bits message_tag;         /* Smob tag. */


/* GC callbacks. */

SCM
mark_message (SCM message)
{
  struct message_data *md = _scm_to_message_data (message);
  return md->session;
}

size_t
free_message (SCM message)
{
  if (! SCM_SMOB_PREDICATE (message_tag, message))
    {
      _ssh_log (SSH_LOG_FUNCTIONS, "free_message", "%s", "already freed");
      return 0;
    }
  struct message_data *msg_data = _scm_to_message_data (message);
  ssh_message_free (msg_data->message);
  return 0;
}


/* Printing procedure. */

static int
print_message (SCM smob,  SCM port, scm_print_state *pstate)
{
  SCM msg_type = guile_ssh_message_get_type (smob);
  scm_puts ("#<message ", port);
  scm_display (msg_type, port);
  scm_puts (" ", port);
  scm_display (_scm_object_hex_address (smob), port);
  scm_puts (">", port);
  return 1;
}


/* Predicates. */

SCM
equalp_message (SCM x1, SCM x2)
{
  struct message_data *msg1 = _scm_to_message_data (x1);
  struct message_data *msg2 = _scm_to_message_data (x2);

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
            "\
Return #t if X a SSH message, #f otherwise.\
")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (message_tag, x));
}


/* Helper procedures. */

SCM
_scm_from_ssh_message (const ssh_message message, SCM session)
{
  SCM smob;
  struct message_data *message_data
    = (struct message_data *) scm_gc_malloc (sizeof (struct message_data),
                                             "message");

  message_data->message = message;
  message_data->session = session;

  SCM_NEWSMOB (smob, message_tag, message_data);
  return smob;
}

/* Convert X to a SSH message. */
struct message_data *
_scm_to_message_data (SCM x)
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
  scm_set_smob_print (message_tag, print_message);
  scm_set_smob_equalp (message_tag, equalp_message);

#include "message-type.x"
}

/* message-type.c ends here */
