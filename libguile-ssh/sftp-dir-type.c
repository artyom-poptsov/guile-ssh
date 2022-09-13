/* sftp-dir-type.c -- SFTP dir type.
 *
 * Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
#include <assert.h>

#include <libguile.h>
#include <libssh/libssh.h>

#include "common.h"
#include "error.h"
#include "sftp-session-type.h"
#include "sftp-dir-type.h"


static const char* GSSH_SFTP_DIR_TYPE_NAME = "sftp-dir";


scm_t_bits sftp_dir_tag;       /* Smob tag. */

gssh_sftp_dir_t*
make_gssh_sftp_dir ()
{
  return (gssh_sftp_dir_t *) scm_gc_malloc (sizeof (gssh_sftp_dir_t),
                                            GSSH_SFTP_DIR_TYPE_NAME);
}

SCM
gssh_sftp_dir_to_scm (sftp_dir dir, SCM path, SCM sftp_session)
{
  gssh_sftp_dir_t* data = make_gssh_sftp_dir ();
  SCM smob;
  data->gssh_sftp_session = sftp_session;
  data->dir = dir;
  data->path = path;
  SCM_NEWSMOB (smob, sftp_dir_tag, data);
  return smob;
}

gssh_sftp_dir_t *
gssh_sftp_dir_from_scm (SCM x)
{
  scm_assert_smob_type (sftp_dir_tag, x);
  return (gssh_sftp_dir_t *) SCM_SMOB_DATA (x);
}

static SCM
_mark (SCM sftp_dir)
{
  gssh_sftp_dir_t* data = gssh_sftp_dir_from_scm (sftp_dir);
  scm_gc_mark (data->path);
  return data->gssh_sftp_session;
}

static size_t
_free (SCM sftp_dir)
{
  gssh_sftp_dir_t* data = gssh_sftp_dir_from_scm (sftp_dir);
  int rc = sftp_closedir (data->dir);
  assert (rc == SSH_NO_ERROR);
  return 0;
}

static SCM
_equalp (SCM x1, SCM x2)
{
  return compare_objects(x1, x2, (converter_t) gssh_sftp_dir_from_scm);
}

static int
_print (SCM sftp_dir, SCM port, scm_print_state* pstate)
{
  scm_puts ("#<sftp-dir ", port);

  scm_display (_scm_object_hex_address (sftp_dir), port);

  scm_putc ('>', port);

  return 1;
}


/* SMOB Initialization. */
void
init_sftp_dir_type (void)
{
  sftp_dir_tag = scm_make_smob_type (GSSH_SFTP_DIR_TYPE_NAME,
                                     sizeof (gssh_sftp_dir_t));
  set_smob_callbacks (sftp_dir_tag, _mark, _free, _equalp, _print);

#include "sftp-dir-type.x"
}



/* sftp-dir-type.c ends here. */
