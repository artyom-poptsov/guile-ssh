/* server-type.c -- SSH server smob.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
#include <libssh/server.h>

#include "common.h"
#include "server-type.h"
#include "server-func.h"

scm_t_bits server_tag;          /* Smob tag. */


/* GC callbacks */

SCM
mark_server (SCM server)
{
  struct server_data *sd = _scm_to_server_data (server);
  return sd->options;
}

size_t
free_server (SCM server)
{
  struct server_data *server_data = _scm_to_server_data (server);
  ssh_bind_free (server_data->bind);
  return 0;
}


/* Smob specific procedures. */

SCM_DEFINE (guile_ssh_make_server, "%make-server", 0, 0, 0,
            (),
            "Make a new SSH server.")
{
  SCM smob;
  struct server_data *server_data
    = (struct server_data *) scm_gc_malloc (sizeof (struct server_data),
                                            "server");
  server_data->bind = ssh_bind_new ();
  server_data->options = SCM_EOL;
  SCM_NEWSMOB (smob, server_tag, server_data);
  return smob;
}


/* Predicates. */

SCM_DEFINE (guile_ssh_is_server_p, "server?", 1, 0, 0,
            (SCM x),
            "Return #t if X is a SSH server, #f otherwise.")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (server_tag, x));
}

SCM
equalp_server (SCM x1, SCM x2)
{
  struct server_data *server1 = _scm_to_server_data (x1);
  struct server_data *server2 = _scm_to_server_data (x2);

  if ((! server1) || (! server2))
    return SCM_BOOL_F;
  else if (server1 != server2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

static int
print_server (SCM server, SCM port, scm_print_state *pstate)
{
  struct server_data *sd = _scm_to_server_data (server);
  SCM bindaddr = scm_assoc_ref (sd->options,
                                _ssh_const_to_scm (server_options,
                                                   SSH_BIND_OPTIONS_BINDADDR));
  SCM bindport = scm_assoc_ref (sd->options,
                                _ssh_const_to_scm (server_options,
                                                   SSH_BIND_OPTIONS_BINDPORT));
  scm_puts ("#<server", port);
  if (scm_is_true (bindaddr))
    {
      scm_putc (' ', port);
      scm_display (bindaddr, port);
    }

  if (scm_is_true (bindport))
    {
      if (scm_is_false (bindaddr))
        scm_putc (' ', port);

      scm_putc (':', port);
      scm_display (bindport, port);
    }

  scm_putc (' ', port);
  scm_display (_scm_object_hex_address (server), port);

  scm_putc ('>', port);

  return 1;
}


/* Helper procedures. */

/* Convert X to a SSH server. */
struct server_data *
_scm_to_server_data (SCM x)
{
  scm_assert_smob_type (server_tag, x);
  return (struct server_data *) SCM_SMOB_DATA (x);
}


/* Server smob initialization. */
void
init_server_type (void)
{
  server_tag = scm_make_smob_type ("server", sizeof (struct server_data));
  scm_set_smob_mark (server_tag, mark_server);
  scm_set_smob_free (server_tag, free_server);
  scm_set_smob_print (server_tag, print_server);
  scm_set_smob_equalp (server_tag, equalp_server);

#include "server-type.x"
}

/* server-type.c ends here */
