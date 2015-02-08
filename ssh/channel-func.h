/* Copyright (C) 2013, 2014, 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __CHANNEL_FUNC_H__
#define __CHANNEL_FUNC_H__

extern SCM guile_ssh_channel_open_session (SCM arg1);
extern SCM guile_ssh_channel_request_exec (SCM arg1, SCM arg2);

extern SCM guile_ssh_channel_is_open_p (SCM arg1);
extern SCM guile_ssh_channel_is_eof_p (SCM arg1);

extern SCM guile_ssh_channel_set_pty_size_x (SCM arg1, SCM arg2, SCM arg3);

extern SCM guile_ssh_channel_get_exit_status (SCM arg1);

extern void init_channel_func (void);

#endif /* ifndef __CHANNEL_FUNC_H__ */
