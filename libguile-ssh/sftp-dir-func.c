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


SCM_GSSH_DEFINE (gssh_sftp_dir_open, "sftp-dir-open", 2,
                 (SCM sftp_session, SCM path))
#define FUNC_NAME s_gssh_sftp_dir_open
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

SCM_GSSH_DEFINE (gssh_sftp_dir_close, "sftp-dir-close", 1,
                 (SCM sftp_dir))
#define FUNC_NAME s_gssh_sftp_dir_close
{
  gssh_sftp_dir_t* dir = gssh_sftp_dir_from_scm (sftp_dir);
  int rc = sftp_closedir (dir->dir);
  if (rc == SSH_ERROR)
    {
      guile_ssh_error1 (FUNC_NAME,
                        "Could not close an SFTP directory",
                        sftp_dir);
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_GSSH_DEFINE (gssh_sftp_dir_eof_p, "sftp-dir-eof?", 1,
                 (SCM sftp_dir))
#define FUNC_NAME s_gssh_sftp_closedir
{
  gssh_sftp_dir_t* dir = gssh_sftp_dir_from_scm (sftp_dir);
  int rc = sftp_dir_eof (dir->dir);
  return scm_from_bool (rc);
}
#undef FUNC_NAME


/* This macro constructs an SCM pair from an SFTP attribute symbol and a C
   integer value. */
#define CONS_INT_ATTR(name, value) \
  scm_cons (gssh_sftp_attr_ ## name, scm_from_int (value))

/* This macro constructs an SCM pair from an SFTP atribute symbol and a C
   string SFTP attribute. */
#define CONS_STR_ATTR(name, value)                                      \
  scm_cons (gssh_sftp_attr_ ## name, scm_from_locale_string (value))

/* This macro constructs an SCM pair from an SFTP attribute symbol and a SSH
   string value. */
#define CONS_SST_ATTR(name, value)  \
  scm_cons (gssh_sftp_attr_ ## name,                                    \
            scm_from_locale_string (ssh_string_to_char (value)))

static SCM
scm_from_sftp_dir_attributes (sftp_attributes attrs)
{
  return scm_list_n (CONS_STR_ATTR (name,                attrs->name),
                     CONS_STR_ATTR (longname,            attrs->longname),
                     CONS_INT_ATTR (flags,               attrs->flags),
                     CONS_INT_ATTR (type,                attrs->type),
                     CONS_INT_ATTR (size,                attrs->size),
                     CONS_INT_ATTR (uid,                 attrs->uid),
                     CONS_INT_ATTR (gid,                 attrs->gid),
                     CONS_STR_ATTR (owner,               attrs->owner),
                     CONS_STR_ATTR (group,               attrs->group),
                     CONS_INT_ATTR (permissions,         attrs->permissions),
                     CONS_INT_ATTR (atime64,             attrs->atime64),
                     CONS_INT_ATTR (atime,               attrs->atime),
                     CONS_INT_ATTR (atime_nseconds,      attrs->atime_nseconds),
                     CONS_INT_ATTR (createtime,          attrs->createtime),
                     CONS_INT_ATTR (createtime_nseconds, attrs->createtime_nseconds),
                     CONS_INT_ATTR (mtime64,             attrs->mtime64),
                     CONS_INT_ATTR (mtime,               attrs->mtime),
                     CONS_INT_ATTR (mtime_nseconds,      attrs->mtime_nseconds),
                     CONS_SST_ATTR (acl,                 attrs->acl),
                     CONS_INT_ATTR (extended_count,      attrs->extended_count),
                     CONS_SST_ATTR (extended_type,       attrs->extended_type),
                     CONS_SST_ATTR (extended_data,       attrs->extended_data),
                     SCM_UNDEFINED);
}

#undef CONS_INT_ATTR
#undef CONS_STR_ATTR
#undef CONS_SST_ATTR

SCM_GSSH_DEFINE (gssh_sftp_dir_read, "sftp-dir-read", 1,
                 (SCM sftp_dir))
#define FUNC_NAME s_gssh_sftp_dir_read
{
  gssh_sftp_dir_t* dir = gssh_sftp_dir_from_scm (sftp_dir);
  gssh_sftp_session_t* session = gssh_sftp_session_from_scm (dir->gssh_sftp_session);
  sftp_attributes attrs = sftp_readdir (session->sftp_session, dir->dir);
  if (attrs == NULL)
    {
      return SCM_BOOL_F;
    }

  return scm_from_sftp_dir_attributes (attrs);
}
#undef FUNC_NAME

/*
  Local Variables:
  c-file-style: "gnu"
  End:
 */

/* sftp-dir-func.c ends here. */
