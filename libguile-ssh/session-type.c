/* session-type.c -- SSH session smob.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <libguile.h>
#include <libssh/libssh.h>
#include <string.h>

#include "session-type.h"
#include "channel-type.h"
#include "session-func.h"
#include "error.h"
#include "common.h"

scm_t_bits session_tag;	/* Smob tag. */

SCM
mark_session (SCM session_smob)
{
  struct session_data *sd = _scm_to_session_data (session_smob);
  return sd->callbacks;
}

/* Handle GC'ing of the session smob. */
size_t
free_session (SCM session_smob)
{
  if (! SCM_SMOB_PREDICATE (session_tag, session_smob))
    {
      _ssh_log (SSH_LOG_FUNCTIONS, "free_session", "%s", "already freed");
      return 0;
    }
  struct session_data *data = _scm_to_session_data (session_smob);

  ssh_disconnect (data->ssh_session);
  ssh_free (data->ssh_session);

  SCM_SET_SMOB_DATA (session_smob, NULL);

  return 0;
}

/* Create a new session. */
SCM_DEFINE (guile_ssh_make_session, "%make-session", 0, 0, 0,
            (),
            "\
Create a new session.\
")
{
  SCM smob;

  struct session_data *session_data
    = (struct session_data *) scm_gc_malloc (sizeof (struct session_data),
                                             "session");

  session_data->ssh_session = ssh_new ();
  if (session_data->ssh_session == NULL)
    return SCM_BOOL_F;

  session_data->callbacks = SCM_BOOL_F;

  SCM_NEWSMOB (smob, session_tag, session_data);

  return smob;
}

static int
print_session (SCM session, SCM port, scm_print_state *pstate)
{
  struct session_data *sd = _scm_to_session_data (session);
  char *user = NULL;
  char *host = NULL;
  unsigned int ssh_port;
  uint32_t smob_addr = (uint32_t) scm_object_address (session);
  int res;

  scm_puts ("#<session ", port);

  res = ssh_options_get (sd->ssh_session, SSH_OPTIONS_USER, &user);
  scm_display ((res == SSH_OK) ? scm_from_locale_string (user) : SCM_UNDEFINED,
               port);
  ssh_string_free_char (user);

  scm_putc ('@', port);

  res = ssh_options_get (sd->ssh_session, SSH_OPTIONS_HOST, &host);
  scm_display ((res == SSH_OK) ? scm_from_locale_string (host) : SCM_UNDEFINED,
               port);
  ssh_string_free_char (host);

  scm_putc (':', port);

  res = ssh_options_get_port (sd->ssh_session, &ssh_port);
  scm_display ((res == SSH_OK) ? scm_from_int (ssh_port) : SCM_UNDEFINED, port);

  scm_puts (ssh_is_connected (sd->ssh_session) ? " (connected) "
                                               : " (disconnected) ",
            port);

  scm_display (_scm_object_hex_address (session), port);

  scm_putc ('>', port);

  return 1;
}


/* Predicates */
SCM_DEFINE (guile_ssh_is_session_p, "session?", 1, 0, 0,
            (SCM x),
            "\
Return #t if X is a SSH session, #f otherwise.\
")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (session_tag, x));
}

SCM
equalp_session (SCM x1, SCM x2)
{
  struct session_data *session1 = _scm_to_session_data (x1);
  struct session_data *session2 = _scm_to_session_data (x2);

  if ((! session1) || (! session2))
    return SCM_BOOL_F;
  else if (session1 != session2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


/* Helper procedures  */

/* Convert SCM object to a SSH session */
struct session_data*
_scm_to_session_data (SCM x)
{
  scm_assert_smob_type (session_tag, x);
  return (struct session_data *) SCM_SMOB_DATA (x);
}


/* session smob initialization. */
void
init_session_type (void)
{
  session_tag = scm_make_smob_type ("session",
                                    sizeof (struct session_data));
  scm_set_smob_mark (session_tag, mark_session);
  scm_set_smob_free (session_tag, free_session);
  scm_set_smob_print (session_tag, print_session);
  scm_set_smob_equalp (session_tag, equalp_session);

#include "session-type.x"
}

/* session.c ends here */
