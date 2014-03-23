/* Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __SERVER_FUNC_H__
#define __SERVER_FUNC_H__

#include <libguile.h>

extern SCM guile_ssh_server_set_x (SCM arg1, SCM arg2, SCM arg3);
extern SCM guile_ssh_server_accept (SCM arg1);

extern void init_server_func (void);

#endif  /* ifndef __SERVER_FUNC_H__ */

/* server-func.h ends here */
