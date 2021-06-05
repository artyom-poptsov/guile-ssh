/* sftp-session-type.c -- SFTP session smob.
 *
 * Copyright (C) 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
#include <libssh/sftp.h>

#include "common.h"
#include "error.h"
#include "session-type.h"
#include "sftp-session-type.h"

scm_t_bits sftp_session_tag;    /* Smob tag. */


/* GC callbacks. */

static SCM
_mark (SCM sftp_session)
{
  gssh_sftp_session_t *sftp_sd = gssh_sftp_session_from_scm (sftp_session);
  return sftp_sd->session;
}

static size_t
_free (SCM sftp_session)
{
  gssh_sftp_session_t *sftp_sd
    = (gssh_sftp_session_t *) SCM_SMOB_DATA (sftp_session);

  sftp_free (sftp_sd->sftp_session);
  return 0;
}

static SCM
_equalp (SCM x1, SCM x2)
{
  gssh_sftp_session_t *sftp_sd1 = gssh_sftp_session_from_scm (x1);
  gssh_sftp_session_t *sftp_sd2 = gssh_sftp_session_from_scm (x2);

  if ((! sftp_sd1) || (! sftp_sd2))
    return SCM_BOOL_F;
  else if (sftp_sd1 != sftp_sd2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

/* Printing procedure. */
static int
_print (SCM sftp_session, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<sftp-session ", port);
  scm_display (_scm_object_hex_address (sftp_session), port);
  scm_puts (">", port);
  return 1;
}


SCM_GSSH_DEFINE (gssh_sftp_session_p, "%gssh-sftp-session?", 1, (SCM x))
{
  return scm_from_bool (SCM_SMOB_PREDICATE (sftp_session_tag, x));
}


SCM_GSSH_DEFINE (gssh_make_sftp_session, "%gssh-make-sftp-session", 1,
                 (SCM session))
#define FUNC_NAME s_gssh_make_sftp_session
{
  gssh_session_t *sd = gssh_session_from_scm (session);
  sftp_session sftp_session = sftp_new (sd->ssh_session);

  if (! sftp_session)
    guile_ssh_error1 (FUNC_NAME, "Could not create a SFTP session", session);

  return make_gssh_sftp_session (sftp_session, session);
}
#undef FUNC_NAME


gssh_sftp_session_t *
gssh_sftp_session_from_scm (SCM x)
{
  scm_assert_smob_type (sftp_session_tag, x);
  return (gssh_sftp_session_t *) SCM_SMOB_DATA (x);
}

SCM
make_gssh_sftp_session (sftp_session sftp_session, SCM session)
{
  SCM smob;
  gssh_sftp_session_t *sftp_sd
    = (gssh_sftp_session_t *) scm_gc_malloc (sizeof (gssh_sftp_session_t),
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
                                         sizeof (gssh_sftp_session_t));
  set_smob_callbacks (sftp_session_tag, _mark, _free, _equalp, _print);

#include "sftp-session-type.x"
}

/* sftp-type.c ends here. */
