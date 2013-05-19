/* Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __KEY_FUNC_H__
#define __KEY_FUNC_H__

SCM guile_ssh_public_key_to_string (SCM key_smob);
SCM guile_ssh_private_key_from_file (SCM session_smob, SCM filename);
SCM guile_ssh_public_key_from_file (SCM session_smob, SCM filename);

void init_key_func (void);

#endif	/* ifndef __KEY_FUNC_H__ */
