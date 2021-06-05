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

#ifndef __SFTP_SESSION_TYPE_H__
#define __SFTP_SESSION_TYPE_H__

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/sftp.h>


extern scm_t_bits sftp_session_tag;


/* Smob data. */
struct gssh_sftp_session {
  /* Reference to the parent session.  We need to keep the reference
     to prevent the session from premature freeing by the GC. */
  SCM session;

  sftp_session sftp_session;
};

typedef struct gssh_sftp_session gssh_sftp_session_t;


extern SCM gssh_sftp_session_p (SCM arg1);
extern SCM gssh_make_sftp_session (SCM arg1);

extern void init_sftp_session_type (void);


/* Internal procedures */

extern gssh_sftp_session_t* gssh_sftp_session_from_scm (SCM x);
extern SCM make_gssh_sftp_session (sftp_session sftp_session, SCM session);

#endif  /* ifndef __SFTP_SESSION_TYPE_H__ */

/* sftp-session-type.h ends here. */
