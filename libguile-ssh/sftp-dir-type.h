/* sftp-dir-type.h -- SFTP dir type description. */

/* Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __SFTP_DIR_TYPE_H__
#define __SFTP_DIR_TYPE_H__


/* Smob data. */
struct gssh_sftp_dir {
    /* Reference to the parent SFTP session. */
    SCM gssh_sftp_session;
    SCM path;

    sftp_dir dir;
};

typedef struct gssh_sftp_dir gssh_sftp_dir_t;

#endif /* ifndef __SFTP_DIR_TYPE_H__ */

/* sftp-dir-type.h ends here. */
