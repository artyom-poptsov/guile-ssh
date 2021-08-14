/* server-type.c -- SSH server smob.
 *
 * Copyright (C) 2013, 2014, 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
#include <libssh/server.h>

#include "common.h"
#include "server-type.h"
#include "server-func.h"

static const char* GSSH_SERVER_TYPE_NAME = "server";


scm_t_bits server_tag;          /* Smob tag. */


/* GC callbacks */

static SCM
_mark (SCM server)
{
  gssh_server_t *sd = gssh_server_from_scm (server);
  return sd->options;
}

static size_t
_free (SCM server)
{
  gssh_server_t *sd = (gssh_server_t *) SCM_SMOB_DATA (server);
  ssh_bind_free (sd->bind);
  return 0;
}

static SCM
_equalp (SCM x1, SCM x2)
{
  return compare_objects(x1, x2, gssh_server_from_scm);
}

static int
_print (SCM server, SCM port, scm_print_state *pstate)
{
    gssh_server_t *sd = gssh_server_from_scm (server);
    SCM bindaddr = scm_assoc_ref (sd->options,
                                  gssh_symbol_to_scm (server_options,
                                                     SSH_BIND_OPTIONS_BINDADDR));
    SCM bindport = scm_assoc_ref (sd->options,
                                  gssh_symbol_to_scm (server_options,
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

gssh_server_t*
make_gssh_server ()
{
  return (gssh_server_t *) scm_gc_malloc (sizeof (gssh_server_t),
                                          GSSH_SERVER_TYPE_NAME);
}


/* Smob specific procedures. */

SCM_DEFINE (guile_ssh_make_server, "%make-server", 0, 0, 0,
            (),
            "Make a new SSH server.")
{
  SCM smob;
  gssh_server_t *server_data = make_gssh_server ();
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


/* Helper procedures. */

/* Convert X to a SSH server. */
gssh_server_t *
gssh_server_from_scm (SCM x)
{
  scm_assert_smob_type (server_tag, x);
  return (gssh_server_t *) SCM_SMOB_DATA (x);
}


/* Server smob initialization. */
void
init_server_type (void)
{
  server_tag = scm_make_smob_type (GSSH_SERVER_TYPE_NAME,
                                   sizeof (gssh_server_t));
  set_smob_callbacks (server_tag, _mark, _free, _equalp, _print);

#include "server-type.x"
}

/* server-type.c ends here */
