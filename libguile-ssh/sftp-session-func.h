/* Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __SFTP_SESSION_FUNC_H__
#define __SFTP_SESSION_FUNC_H__

extern SCM gssh_sftp_init (SCM sftp_session);
extern SCM gssh_sftp_get_session (SCM sftp_session);
extern SCM gssh_sftp_mkdir (SCM sftp_session, SCM dirname, SCM mode);
extern SCM gssh_sftp_rmdir (SCM sftp_session, SCM dirname);
extern SCM gssh_sftp_mv (SCM sftp_session, SCM source, SCM dest);
extern SCM gssh_sftp_chmod (SCM sftp_session, SCM filename, SCM mode);
extern SCM gssh_sftp_symlink (SCM sftp_session, SCM target, SCM dest);
extern SCM gssh_sftp_readlink (SCM sftp_session, SCM path);
extern SCM gssh_sftp_unlink (SCM sftp_session, SCM path);
extern SCM gssh_sftp_get_error (SCM sftp_session);


extern void init_sftp_session_func (void);

#endif /* ifndef __SFTP_SESSION_FUNC_H__ */
