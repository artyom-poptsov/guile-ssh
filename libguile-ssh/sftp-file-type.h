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

extern scm_t_bits sftp_file_tag;


/* Smob data. */
struct sftp_file_data {
  /* Reference to the parent SFTP session. */
  SCM sftp_session;

  sftp_file file;
};


extern struct sftp_file_data * _scm_to_sftp_file_data (SCM x);
extern SCM _scm_from_sftp_file (const sftp_file file,
                                const SCM name,
                                SCM sftp_session);

/* sftp-file-type.h ends here. */
