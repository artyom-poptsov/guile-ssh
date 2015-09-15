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
#include "common.h"
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

SCM_DEFINE (gssh_sftp_rmdir, "%gssh-sftp-rmdir", 2, 0, 0,
            (SCM sftp_session, SCM dirname),
            "")
#define FUNC_NAME s_gssh_sftp_rmdir
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  char *c_dirname;

  SCM_ASSERT (scm_is_string (dirname), dirname, SCM_ARG2, FUNC_NAME);

  scm_dynwind_begin (0);

  c_dirname = scm_to_locale_string (dirname);
  scm_dynwind_free (c_dirname);

  if (sftp_rmdir (sftp_sd->sftp_session, c_dirname))
    {
      guile_ssh_error1 (FUNC_NAME, "Could not remove a directory",
                        scm_list_2 (sftp_session, dirname));
    }

  scm_dynwind_end ();

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (gssh_sftp_mv, "%gssh-sftp-mv", 3, 0, 0,
            (SCM sftp_session, SCM source, SCM dest),
            "")
#define FUNC_NAME s_gssh_sftp_mv
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  char *c_source;
  char *c_dest;

  SCM_ASSERT (scm_is_string (source), source, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_string (dest),   dest,   SCM_ARG3, FUNC_NAME);

  scm_dynwind_begin (0);

  c_source = scm_to_locale_string (source);
  scm_dynwind_free (c_source);

  c_dest = scm_to_locale_string (dest);
  scm_dynwind_free (c_dest);

  if (sftp_rename (sftp_sd->sftp_session, c_source, c_dest))
    {
      guile_ssh_error1 (FUNC_NAME, "Could not move a file",
                        scm_list_3 (sftp_session, source, dest));
    }

  scm_dynwind_end ();

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (gssh_sftp_chmod, "%gssh-sftp-chmod", 3, 0, 0,
            (SCM sftp_session, SCM filename, SCM mode),
            "")
#define FUNC_NAME s_gssh_sftp_chmod
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  char *c_filename;

  SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_number (mode), mode, SCM_ARG3, FUNC_NAME);

  scm_dynwind_begin (0);

  c_filename = scm_to_locale_string (filename);
  scm_dynwind_free (c_filename);

  if (sftp_chmod (sftp_sd->sftp_session, c_filename, scm_to_uint32 (mode)))
    {
      guile_ssh_error1 (FUNC_NAME, "Could not chmod a file",
                        scm_list_3 (sftp_session, filename, mode));
    }

  scm_dynwind_end ();

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (gssh_sftp_symlink, "%gssh-sftp-symlink", 3, 0, 0,
            (SCM sftp_session, SCM target, SCM dest),
            "")
#define FUNC_NAME s_gssh_sftp_symlink
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  char *c_target;
  char *c_dest;

  SCM_ASSERT (scm_is_string (target), target, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_string (dest),   dest,   SCM_ARG3, FUNC_NAME);

  scm_dynwind_begin (0);

  c_target = scm_to_locale_string (target);
  scm_dynwind_free (c_target);

  c_dest = scm_to_locale_string (dest);
  scm_dynwind_free (c_dest);

  if (sftp_symlink (sftp_sd->sftp_session, c_target, c_dest))
    {
      guile_ssh_error1 (FUNC_NAME, "Could not create a symlink",
                        scm_list_3 (sftp_session, target, dest));
    }

  scm_dynwind_end ();

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (gssh_sftp_readlink, "%gssh-sftp-readlink", 2, 0, 0,
            (SCM sftp_session, SCM path),
            "")
#define FUNC_NAME s_gssh_sftp_readlink
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  char *c_path;
  char *ret;

  SCM_ASSERT (scm_is_string (path), path, SCM_ARG2, FUNC_NAME);

  scm_dynwind_begin (0);

  c_path = scm_to_locale_string (path);
  scm_dynwind_free (c_path);

  ret = sftp_readlink (sftp_sd->sftp_session, c_path);

  scm_dynwind_end ();

  return ret ? scm_take_locale_string (ret) : SCM_BOOL_F;
}
#undef FUNC_NAME


/* Possible SFTP return codes. */
static struct symbol_mapping sftp_return_codes[] = {
  { "fx-ok",                 SSH_FX_OK                  },
  { "fx-eof",                SSH_FX_EOF                 },
  { "fx-no-such-file",       SSH_FX_NO_SUCH_FILE        },
  { "fx-permission-denied",  SSH_FX_PERMISSION_DENIED   },
  { "fx-failure",            SSH_FX_FAILURE             },
  { "fx-bad-message",        SSH_FX_BAD_MESSAGE         },
  { "fx-no-connection",      SSH_FX_NO_CONNECTION       },
  { "fx-connection-lost",    SSH_FX_CONNECTION_LOST     },
  { "fx-op-unsupported",     SSH_FX_OP_UNSUPPORTED      },
  { "fx-invalid-handle",     SSH_FX_INVALID_HANDLE      },
  { "fx-no-such-path",       SSH_FX_NO_SUCH_PATH        },
  { "fx-file-already-exist", SSH_FX_FILE_ALREADY_EXISTS },
  { "fx-write-protect",      SSH_FX_WRITE_PROTECT       },
  { "fx-no-media",           SSH_FX_NO_MEDIA            },
  { NULL,                   -1                          }
};

SCM_DEFINE (gssh_sftp_get_error, "%gssh-sftp-get-error", 1, 0, 0,
            (SCM sftp_session),
            "")
#define FUNC_NAME s_gssh_sftp_get_error
{
  struct sftp_session_data *sftp_sd = _scm_to_sftp_session_data (sftp_session);
  int rc = sftp_get_error (sftp_sd->sftp_session);
  if (rc < 0)
    {
      guile_ssh_error1 (FUNC_NAME, "Could not get an error code",
                        sftp_session);
    }

  return _ssh_const_to_scm (sftp_return_codes, rc);
}
#undef FUNC_NAME


void
init_sftp_session_func (void)
{
#include "sftp-session-func.x"
}

/* sftp-session-func.c ends here. */
