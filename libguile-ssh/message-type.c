/* message-type.c -- SSH message smob.
 *
 * Copyright (C) 2013, 2014, 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <config.h>

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/server.h>

#include "message-type.h"
#include "message-func.h"
#include "common.h"

static const char* GSSH_MESSAGE_TYPE_NAME = "message";

scm_t_bits message_tag;         /* Smob tag. */


/* GC callbacks. */

static SCM
_mark (SCM message)
{
  gssh_message_t* md = _scm_to_message_data (message);
  return md->session;
}

static size_t
_free (SCM message)
{
  gssh_message_t* md = (gssh_message_t *) SCM_SMOB_DATA (message);
  ssh_message_free (md->message);
  return 0;
}


/* Printing procedure. */

static int
_print (SCM smob,  SCM port, scm_print_state *pstate)
{
  SCM msg_type = guile_ssh_message_get_type (smob);
  scm_puts ("#<message ", port);
  scm_display (msg_type, port);
  scm_puts (" ", port);
  scm_display (_scm_object_hex_address (smob), port);
  scm_puts (">", port);
  return 1;
}

SCM
_equalp (SCM x1, SCM x2)
{
  return compare_objects(x1, x2, (converter_t) _scm_to_message_data);
}


/* Predicates. */

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
  gssh_message_t* message_data
    = (gssh_message_t *) scm_gc_malloc (sizeof (gssh_message_t),
                                        GSSH_MESSAGE_TYPE_NAME);

  message_data->message = message;
  message_data->session = session;

  SCM_NEWSMOB (smob, message_tag, message_data);
  return smob;
}

/* Convert X to a SSH message. */
gssh_message_t *
_scm_to_message_data (SCM x)
{
  scm_assert_smob_type (message_tag, x);
  return (gssh_message_t *) SCM_SMOB_DATA (x);
}


/* Message smob initialization. */
void
init_message_type (void)
{
  message_tag = scm_make_smob_type (GSSH_MESSAGE_TYPE_NAME,
                                    sizeof (gssh_message_t));
  set_smob_callbacks (message_tag, _mark, _free, _equalp, _print);

#include "message-type.x"
}

/* message-type.c ends here */
