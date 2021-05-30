/* sftp-file-type.h -- SFTP file type description. */

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

#ifndef __SFTP_FILE_TYPE_H__
#define __SFTP_FILE_TYPE_H__

#include <libguile.h>
#include <libssh/sftp.h>


/* Smob data. */
struct gssh_sftp_file {
  /* Reference to the parent SFTP session. */
  SCM sftp_session;

  sftp_file file;
};

typedef struct gssh_sftp_file gssh_sftp_file_t;


extern SCM gssh_sftp_open (SCM sftp_session, SCM path, SCM access_type,
                           SCM mode);
extern SCM gssh_sftp_file_p (SCM x);


extern void init_sftp_file_type (void);


extern gssh_sftp_file_t* _scm_to_sftp_file_data (SCM x);
extern SCM make_gssh_sftp_file (const sftp_file file,
                                const SCM name,
                                SCM sftp_session);

#endif /* ifndef __SFTP_FILE_TYPE_H__ */

/* sftp-file-type.h ends here. */
