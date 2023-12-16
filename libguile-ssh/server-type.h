/* Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __SERVER_TYPE_H__
#define __SERVER_TYPE_H__

#include <libguile.h>
#include <libssh/server.h>

extern scm_t_bits server_tag;


/* Smob data. */
struct gssh_server {
  ssh_bind bind;
  SCM options;
  SCM callbacks;
};

typedef struct gssh_server gssh_server_t;

extern gssh_server_t* make_gssh_server ();

extern SCM guile_ssh_is_server_p (SCM arg1);
extern void init_server_type (void);


/* Helper procedures. */
extern gssh_server_t* gssh_server_from_scm (SCM x);

#endif  /* ifndef __SERVER_TYPE_H__ */

/* server-type.h ends here */
