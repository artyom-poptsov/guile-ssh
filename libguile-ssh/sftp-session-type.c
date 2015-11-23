/* sftp-session-type.c -- SFTP session smob.
 *
 * Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
#include <libssh/sftp.h>

#include "common.h"
#include "session-type.h"
#include "sftp-session-type.h"

scm_t_bits sftp_session_tag;    /* Smob tag. */


/* GC callbacks. */

static SCM
mark_sftp_session (SCM sftp_session)
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  return sftp_sd->session;
}

static size_t
free_sftp_session (SCM sftp_session)
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  sftp_free (sftp_sd->sftp_session);
  return 0;
}


/* Printing procedure. */
static int
print_sftp_session (SCM sftp_session, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<sftp-session ", port);
  scm_display (_scm_object_hex_address (sftp_session), port);
  scm_puts (">", port);
}


static SCM
equalp_sftp_session (SCM x1, SCM x2)
{
  struct sftp_session_data *sftp_sd1 = _scm_to_sftp_session_data (x1);
  struct sftp_session_data *sftp_sd2 = _scm_to_sftp_session_data (x2);

  if ((! sftp_sd1) || (! sftp_sd2))
    return SCM_BOOL_F;
  else if (sftp_sd1 != sftp_sd2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM_GSSH_DEFINE (gssh_sftp_session_p, "%gssh-sftp-session?", 1, (SCM x))
{
  return scm_from_bool (SCM_SMOB_PREDICATE (sftp_session_tag, x));
}


SCM_GSSH_DEFINE (gssh_make_sftp_session, "%gssh-make-sftp-session", 1,
                 (SCM session))
#define FUNC_NAME s_gssh_make_sftp_session
{
  struct session_data *sd = _scm_to_session_data (session);
  sftp_session sftp_session = sftp_new (sd->ssh_session);

  if (! sftp_session)
    guile_ssh_error1 (FUNC_NAME, "Could not create a SFTP session", session);

  return _scm_from_sftp_session (sftp_session, session);
}
#undef FUNC_NAME


struct sftp_session_data *
_scm_to_sftp_session_data (SCM x)
{
  scm_assert_smob_type (sftp_session_tag, x);
  return (struct sftp_session_data *) SCM_SMOB_DATA (x);
}

SCM
_scm_from_sftp_session (sftp_session sftp_session, SCM session)
{
  SCM smob;
  struct sftp_session_data *sftp_sd
    = (struct sftp_session_data *) scm_gc_malloc (sizeof
                                                  (struct sftp_session_data),
                                                  "sftp session");
  sftp_sd->sftp_session = sftp_session;
  sftp_sd->session      = session;
  SCM_NEWSMOB (smob, sftp_session_tag, sftp_sd);
  return smob;
}

void
init_sftp_session_type (void)
{
  sftp_session_tag = scm_make_smob_type ("sftp session",
                                         sizeof (struct sftp_session_data));
  scm_set_smob_mark (sftp_session_tag, mark_sftp_session);
  scm_set_smob_free (sftp_session_tag, free_sftp_session);
  scm_set_smob_print (sftp_session_tag, print_sftp_session);
  scm_set_smob_equalp (sftp_session_tag, equalp_sftp_session);

#include "sftp-session-type.x"
}

/* sftp-type.c ends here. */
