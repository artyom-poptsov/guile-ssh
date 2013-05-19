/* channel-type.c -- SSH channel smob.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <libguile.h>
#include <libssh/libssh.h>

#include "session-type.h"
#include "channel-type.h"
#include "error.h"

scm_t_bits channel_tag;	/* Smob tag. */


/* Smob specific procedures */

SCM
mark_channel (SCM channel_smob)
{
  return SCM_BOOL_F;
}

size_t
free_channel (SCM channel_smob)
{
  struct channel_data *data
    = (struct channel_data *) SCM_SMOB_DATA (channel_smob);

  ssh_channel_free (data->ssh_channel);

  return 0;
}

/* Allocate a new SSH channel. */
SCM
guile_ssh_make_channel (SCM session_smob)
{
  SCM smob;

  struct session_data *session_data
    = (struct session_data *) SCM_SMOB_DATA (session_smob);

  struct channel_data *channel_data 
    = (struct channel_data *) scm_gc_malloc (sizeof (struct channel_data),
					     "channel");
  channel_data->ssh_channel = ssh_channel_new (session_data->ssh_session);
  if (channel_data->ssh_channel == NULL)
    return SCM_BOOL_F;

  SCM_NEWSMOB (smob, channel_tag, channel_data);

  return smob;
}


/* Predicates */

SCM
guile_ssh_is_channel_p (SCM obj)
{
  return scm_from_bool (SCM_SMOB_PREDICATE (channel_tag, obj));
}


/* channel smob initialization. */
void
init_channel_type (void)
{
  channel_tag = scm_make_smob_type ("ssh:channel", sizeof (struct channel_data));
  scm_set_smob_mark (channel_tag, mark_channel);
  scm_set_smob_free (channel_tag, free_channel); 

  scm_c_define_gsubr ("ssh:make-channel", 1, 0, 0, guile_ssh_make_channel);

  scm_c_define_gsubr ("ssh:channel?",     1, 0, 0, guile_ssh_is_channel_p);
}

/* channel-type.c ends here */
