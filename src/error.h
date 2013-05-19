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

#ifndef __GUILE_SSH_ERROR_H__
#define __GUILE_SSH_ERROR_H__

#define GUILE_SSH_ERROR "guile-ssh-error"

inline void guile_ssh_error (const char *proc, const char *msg, SCM args, SCM rest);
inline void guile_ssh_error1 (const char *proc, const char *msg, SCM args);

#endif /* ifndef __GUILE_SSH_ERROR_H__ */
