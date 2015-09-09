/* sftp-session-func.c -- Functions for working with SFTP sessions.
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


/* Guile */
#include <libguile.h>

/* libssh */
#include <libssh/libssh.h>
#include <libssh/sftp.h>

/* Guile-SSH */
#include "sftp-session-type.h"


SCM_DEFINE (gssh_sftp_init, "%gssh-sftp-init", 1, 0, 0,
            (SCM sftp_session),
            "")
#define FUNC_NAME s_gssh_sftp_init
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  if (sftp_init (sftp_sd->sftp_session))
    {
      guile_ssh_error1 (FUNC_NAME, "Could not initialize the SFTP session.",
                        sftp_session);
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (gssh_sftp_get_session, "%gssh-sftp-get-session", 1, 0, 0,
            (SCM sftp_session),
            "")
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  return sftp_sd->session;
}


SCM_DEFINE (gssh_sftp_mkdir, "%gssh-sftp-mkdir", 3, 0, 0,
            (SCM sftp_session, SCM dirname, SCM mode),
            "")
#define FUNC_NAME s_gssh_sftp_mkdir
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  char *c_dirname;

  SCM_ASSERT (scm_is_string (dirname), dirname, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_number (mode), mode, SCM_ARG3, FUNC_NAME);

  scm_dynwind_begin (0);

  c_dirname = scm_to_locale_string (dirname);
  scm_dynwind_free (c_dirname);

  if (sftp_mkdir (sftp_sd->sftp_session, c_dirname, scm_to_uint32 (mode)))
    {
      guile_ssh_error1 (FUNC_NAME, "Could not create a directory",
                        scm_list_3 (sftp_session, dirname, mode));
    }

  scm_dynwind_end ();
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


void
init_sftp_session_func (void)
{
#include "sftp-session-func.x"
}

/* sftp-session-func.c ends here. */
