/* sftp-dir-func.c -- Functions for working with SFTP directories.
 *
 * Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

/* Guile */
#include <libguile.h>

/* libssh */
#include <libssh/libssh.h>
#include <libssh/sftp.h>

/* Guile-SSH */
#include "common.h"
#include "error.h"
#include "sftp-session-type.h"
#include "sftp-dir-type.h"


SCM_GSSH_DEFINE (gssh_sftp_opendir, "sftp-opendir", 2,
                 (SCM sftp_session, SCM path))
#define FUNC_NAME s_gssh_sftp_opendir
{
  gssh_sftp_session_t* data = gssh_sftp_session_from_scm (sftp_session);
  sftp_dir dir;
  char* c_path;

  scm_dynwind_begin (0);

  c_path = scm_to_locale_string (path);
  scm_dynwind_free (c_path);

  dir = sftp_opendir (data->sftp_session, c_path);
  if (dir == NULL)
    {
      guile_ssh_error1 (FUNC_NAME,
                        "Could not open a directory",
                        scm_list_2 (sftp_session, path));
    }
  scm_dynwind_end ();

  return gssh_sftp_dir_to_scm (dir, path, sftp_session);
}
#undef FUNC_NAME

/*
  Local Variables:
  c-file-style: "gnu"
  End:
 */

/* sftp-dir-func.c ends here. */
