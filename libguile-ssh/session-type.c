/* session-type.c -- SSH session smob.
 *
 * Copyright (C) 2013, 2014, 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <config.h>

#include <libguile.h>
#include <libssh/libssh.h>
#include <string.h>

#include "session-type.h"
#include "channel-type.h"
#include "session-func.h"
#include "error.h"
#include "common.h"

scm_t_bits session_tag;	/* Smob tag. */

static SCM
_mark (SCM session_smob)
{
  gssh_session_t *sd = gssh_session_from_scm (session_smob);
  return sd->callbacks;
}

/* Handle GC'ing of the session smob. */
static size_t
_free (SCM session)
{
  gssh_session_t *sd = (gssh_session_t *) SCM_SMOB_DATA (session);

  ssh_disconnect (sd->ssh_session);
  ssh_free (sd->ssh_session);

  SCM_SET_SMOB_DATA (session, NULL);

  return 0;
}

static SCM
_equalp (SCM x1, SCM x2)
{
  gssh_session_t *session1 = gssh_session_from_scm (x1);
  gssh_session_t *session2 = gssh_session_from_scm (x2);

  if ((! session1) || (! session2))
    return SCM_BOOL_F;
  else if (session1 != session2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

static int
_print (SCM session, SCM port, scm_print_state *pstate)
{
  gssh_session_t *sd = gssh_session_from_scm (session);
  char *user = NULL;
  char *host = NULL;
  unsigned int ssh_port;
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


/* Create a new session. */
SCM_DEFINE (guile_ssh_make_session, "%make-session", 0, 0, 0,
            (),
            "\
Create a new session.\
")
{
  SCM smob;

  gssh_session_t *session_data
    = (gssh_session_t *) scm_gc_malloc (sizeof (gssh_session_t),
                                        "session");

  session_data->ssh_session = ssh_new ();
  if (session_data->ssh_session == NULL)
    return SCM_BOOL_F;

  session_data->callbacks = SCM_BOOL_F;

  SCM_NEWSMOB (smob, session_tag, session_data);

  return smob;
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


/* Helper procedures  */

/* Convert SCM object to a SSH session */
gssh_session_t*
gssh_session_from_scm (SCM x)
{
  scm_assert_smob_type (session_tag, x);
  return (gssh_session_t *) SCM_SMOB_DATA (x);
}


/* session smob initialization. */
void
init_session_type (void)
{
  session_tag = scm_make_smob_type ("session",
                                    sizeof (gssh_session_t));
  set_smob_callbacks (session_tag, _mark, _free, _equalp, _print);

#include "session-type.x"
}

/* session.c ends here */
