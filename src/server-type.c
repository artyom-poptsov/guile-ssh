/* server-type.c -- SSH server smob.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of libguile-ssh
 *
 * libguile-ssh is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * libguile-ssh is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libguile-ssh.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/server.h>

#include "server-type.h"

scm_t_bits server_tag;          /* Smob tag. */


/* GC callbacks */

SCM
mark_server (SCM server)
{
  return SCM_BOOL_F;
}

size_t
free_server (SCM server)
{
  struct server_data *server_data = _scm_to_ssh_server (server);
  ssh_bind_free (server_data->bind);
  return 0;
}


/* Smob specific procedures. */

SCM_DEFINE (guile_ssh_make_server, "ssh:make-server", 0, 0, 0,
            (),
            "Make a new SSH server.")
{
  SCM smob;
  struct server_data *server_data
    = (struct server_data *) scm_gc_malloc (sizeof (struct server_data),
                                            "server");
  server_data->bind = ssh_bind_new ();
  SCM_NEWSMOB (smob, server_tag, server_data);
  return smob;
}


SCM_DEFINE (guile_ssh_server_close_x, "ssh:server-close!", 1, 0, 0,
            (SCM server),
            "Close the SSH server SERVER.\n"
            "Return value is undefined.")
{
  struct server_data *server_data = _scm_to_ssh_server (server);
  ssh_bind_free (server_data->bind);
  return SCM_UNDEFINED;
}


/* Predicates. */

SCM
equalp_server (SCM x1, SCM x2)
{
  struct server_data *server1 = _scm_to_ssh_server (x1);
  struct server_data *server2 = _scm_to_ssh_server (x2);

  if ((! server1) || (! server2))
    return SCM_BOOL_F;
  else if (server1 != server2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


/* Helper procedures. */

/* Convert X to a SSH server. */
struct server_data *
_scm_to_ssh_server (SCM x)
{
  scm_assert_smob_type (server_tag, x);
  return (struct server_data *) SCM_SMOB_DATA (x);
}


/* Server smob initialization. */
void
init_server_type (void)
{
  server_tag = scm_make_smob_type ("ssh:server", sizeof (struct server_data));
  scm_set_smob_mark (server_tag, mark_server);
  scm_set_smob_free (server_tag, free_server);
  scm_set_smob_equalp (server_tag, equalp_server);

#include "server-type.x"
}

/* server-type.c ends here */
