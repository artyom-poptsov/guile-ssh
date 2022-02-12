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


static const char* GSSH_SESSION_TYPE_NAME = "session";


scm_t_bits session_tag;	/* Smob tag. */

static SCM
_mark (SCM session_smob)
{
  if (! gssh_session_freed_p (session_smob))
    {
      gssh_session_t *sd = gssh_session_from_scm (session_smob);
      scm_gc_mark (sd->channels);
      return sd->callbacks;
    }
  else
    {
      return SCM_BOOL_F;
    }
}

/* Handle GC'ing of the session smob. */
static size_t
_free (SCM session)
{
  if (! gssh_session_freed_p (session))
    {
      gssh_session_t *sd = gssh_session_from_scm (session);
      guile_ssh_disconnect (session);
      ssh_free (sd->ssh_session);
      SCM_SET_SMOB_DATA (session, NULL);
    }

  return 0;
}

static SCM
_equalp (SCM x1, SCM x2)
{
  return compare_objects(x1, x2, (converter_t) gssh_session_from_scm);
}

static int
_print (SCM session, SCM port, scm_print_state *pstate)
{
  char *user = NULL;
  char *host = NULL;
  unsigned int ssh_port;
  int res;

  scm_puts ("#<session ", port);
  if (! gssh_session_freed_p (session))
    {
      gssh_session_t *sd = gssh_session_from_scm (session);
      res = ssh_options_get (sd->ssh_session, SSH_OPTIONS_USER, &user);
      scm_display ((res == SSH_OK)
                   ? scm_from_locale_string (user)
                   : SCM_UNDEFINED,
                   port);
      ssh_string_free_char (user);

      scm_putc ('@', port);

      res = ssh_options_get (sd->ssh_session, SSH_OPTIONS_HOST, &host);
      scm_display ((res == SSH_OK)
                   ? scm_from_locale_string (host)
                   : SCM_UNDEFINED,
                   port);
      ssh_string_free_char (host);

      scm_putc (':', port);

      res = ssh_options_get_port (sd->ssh_session, &ssh_port);
      scm_display ((res == SSH_OK)
                   ? scm_from_int (ssh_port)
                   : SCM_UNDEFINED,
                   port);

      scm_puts (ssh_is_connected (sd->ssh_session) ? " (connected) "
                : " (disconnected) ",
                port);
    }
  else
    {
      scm_puts ("(freed) ", port);
    }

  scm_display (_scm_object_hex_address (session), port);

  scm_putc ('>', port);

  return 1;
}


/* Internal procedure.

   Add a CHANNEL to the channel list of a SESSION. */
void
gssh_session_add_channel_x (gssh_session_t* session, SCM channel)
{
  session->channels = scm_cons (channel, session->channels);
}

/* Internal procedure. */
void
gssh_session_del_channel_x (gssh_session_t* session, SCM channel)
{
  session->channels = scm_delete (channel, session->channels);
}

/* Internal procedure. */
void
gssh_session_close_all_channels_x (gssh_session_t* session)
{
  int32_t length;
  while ((length = scm_to_int (scm_length (session->channels))) > 0) {
    scm_close_port (scm_list_ref (session->channels, scm_from_int (0)));
  }
}

/**
 * Internal procedure.
 *
 * Create a Guile-SSH session object.
 */
gssh_session_t*
make_gssh_session ()
{
  return (gssh_session_t *) scm_gc_malloc (sizeof (gssh_session_t),
                                           GSSH_SESSION_TYPE_NAME);
}


/* Helper procedures  */

SCM
gssh_session_to_scm (gssh_session_t* session)
{
  SCM smob;
  SCM_NEWSMOB (smob, session_tag, session);
  return smob;
}

/* Convert SCM object to a SSH session */
gssh_session_t*
gssh_session_from_scm (SCM x)
#define FUNC_NAME "gssh_session_from_scm"
{
  scm_assert_smob_type (session_tag, x);
  return (gssh_session_t *) SCM_SMOB_DATA (x);
}
#undef FUNC_NAME

/**
 * Check if a SESSION is freed.
 */
int
gssh_session_freed_p (SCM session)
{
  return SCM_SMOBNUM (session) == 0;
}


/* session smob initialization. */
void
init_session_type (void)
{
  session_tag = scm_make_smob_type (GSSH_SESSION_TYPE_NAME,
                                    sizeof (gssh_session_t));
  set_smob_callbacks (session_tag, _mark, _free, _equalp, _print);

#include "session-type.x"
}

/* session.c ends here */
