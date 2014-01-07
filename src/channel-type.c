/* channel-type.c -- SSH channel smob.
 *
 * Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
#include <assert.h>

#include "session-type.h"
#include "channel-type.h"
#include "error.h"

scm_t_bits channel_tag;         /* Smob tag. */

enum { PORT_BUFSZ = 256 };      /* Size of the port's buffer */


/* Ptob specific procedures */

/* Read data from the channel. 

   Return EOF if no data is available or an error occured */
static int
ptob_fill_input (SCM port)
#define FUNC_NAME "ptob_fill_input"
{
  struct channel_data *cd = _scm_to_ssh_channel (port);
  scm_port *pt = SCM_PTAB_ENTRY (port);
  int res;

  res = ssh_channel_read (cd->ssh_channel,
                          pt->read_buf, pt->read_buf_size,
                          cd->is_stderr);

  if (res == SSH_ERROR)
    guile_ssh_error1 (FUNC_NAME, "Error reading from the channel", port);

  if ((! res) || (res == SSH_AGAIN))
    return EOF;

  pt->read_pos = pt->read_buf;
  pt->read_end = pt->read_buf + res;

  return *pt->read_buf;
}
#undef FUNC_NAME

/* Write data to the channel.  Throw `guile-ssh-error' on a libssh
   error, or signal a system error if amount of data written is
   smaller than size SZ. */
static void
ptob_write (SCM channel, const void *data, size_t sz)
#define FUNC_NAME "ptob_write"
{
  struct channel_data *channel_data = _scm_to_ssh_channel (channel);
  int res = ssh_channel_write (channel_data->ssh_channel, data, sz);
  if (res == SSH_ERROR)
    {
      ssh_session session = ssh_channel_get_session (channel_data->ssh_channel);
      guile_ssh_error1 (FUNC_NAME, ssh_get_error (session), channel);
    }

  if (res < sz)
    scm_syserror (FUNC_NAME);
}
#undef FUNC_NAME

static void
ptob_flush (SCM channel)
#define FUNC_NAME "ptob_flush"
{
  scm_port *pt = SCM_PTAB_ENTRY (channel);
  struct channel_data *cd = _scm_to_ssh_channel (channel);
  size_t wrsize = pt->write_pos - pt->write_buf;

  if (wrsize)
    {
      int res = ssh_channel_write (cd->ssh_channel, pt->write_buf, wrsize);
      if (res == SSH_ERROR)
        {
          ssh_session session = ssh_channel_get_session (cd->ssh_channel);
          guile_ssh_error1 (FUNC_NAME, ssh_get_error (session), channel);
        }
    }

  pt->write_pos = pt->write_buf;
}
#undef FUNC_NAME

static int
ptob_input_waiting (SCM channel)
#define FUNC_NAME "ptob_input_waiting"
{
  struct channel_data *cd = _scm_to_ssh_channel (channel);
  int res = ssh_channel_poll (cd->ssh_channel, cd->is_stderr);
  if (res < 0)
    guile_ssh_error1 (FUNC_NAME, "An error occured.", channel);

  return res;
}
#undef FUNC_NAME

static int
ptob_close (SCM channel)
{
  scm_port *pt = SCM_PTAB_ENTRY (channel);
  struct channel_data *ch = _scm_to_ssh_channel (channel);

  ptob_flush (channel);

  if (ch)
    {
      ssh_channel_close (ch->ssh_channel);
      ssh_channel_free (ch->ssh_channel);
    }

  scm_gc_free (ch, sizeof (struct channel_data), "channel");
  scm_gc_free (pt->write_buf, PORT_BUFSZ, "port write buffer");
  scm_gc_free (pt->read_buf,  PORT_BUFSZ, "port read buffer");
  SCM_SETSTREAM (channel, NULL);

  return 0;
}

SCM
mark_channel (SCM channel_smob)
{
  return SCM_BOOL_F;
}

size_t
free_channel (SCM channel_smob)
{
  /* All resources allocated by the SSH channel will be freed along
     with the related SSH session. */
  return 0;
}

static int
print_channel (SCM smob,  SCM port, scm_print_state *pstate)
{
  if (scm_is_false (scm_port_closed_p (smob)))
    {
      struct channel_data *ch = _scm_to_ssh_channel (smob);
      int is_open = ssh_channel_is_open (ch->ssh_channel);
      scm_puts ("#<", port);
      scm_puts (is_open ? "open" : "closed", port);
      scm_puts (" ssh channel>", port);
    }
  else
    {
      scm_puts ("#<closed ssh channel>", port);
    }
  return 1;
}

/* Pack the SSH channel CH to a Scheme port and return newly created
   port. */
SCM
_ssh_channel_to_scm (ssh_channel ch)
{
  struct channel_data *channel_data;
  SCM ptob;
  scm_port *pt;
  
  channel_data = scm_gc_malloc (sizeof (struct channel_data), "channel");

  channel_data->ssh_channel = ch;
  channel_data->is_stderr = 0;  /* Reading from stderr disabled by default */

  ptob = scm_new_port_table_entry (channel_tag);
  pt   = SCM_PTAB_ENTRY (ptob);

  pt->rw_random = 0;

  /* Output init */
  pt->write_buf = scm_gc_malloc (PORT_BUFSZ, "port write buffer");
  pt->write_buf_size = PORT_BUFSZ;
  pt->write_pos = pt->write_buf;
  pt->write_end = pt->write_buf;

  /* Input init */
  pt->read_buf = scm_gc_malloc (PORT_BUFSZ, "port read buffer");
  pt->read_buf_size = PORT_BUFSZ;
  pt->read_pos = pt->read_buf;
  pt->read_end = pt->read_buf;

  SCM_SET_CELL_TYPE (ptob, channel_tag | SCM_RDNG | SCM_WRTNG);
  SCM_SETSTREAM (ptob, channel_data);

  return ptob;
}

/* Allocate a new SSH channel. */
SCM_DEFINE (guile_ssh_make_channel, "make-channel", 1, 0, 0,
            (SCM arg1),
            "Allocate a new SSH channel.")
{
  struct session_data *session_data = _scm_to_ssh_session (arg1);
  ssh_channel ch = ssh_channel_new (session_data->ssh_session);

  if (! ch)
    return SCM_BOOL_F;

  return _ssh_channel_to_scm (ch);
}


/* Predicates */

SCM_DEFINE (guile_ssh_is_channel_p, "channel?", 1, 0, 0,
            (SCM x),
            "Return #t if X is a SSH channel, #f otherwise.")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (channel_tag, x));
}

SCM
equalp_channel (SCM x1, SCM x2)
{
  struct channel_data *channel1 = _scm_to_ssh_channel (x1);
  struct channel_data *channel2 = _scm_to_ssh_channel (x2);

  if ((! channel1) || (! channel2))
    return SCM_BOOL_F;
  else if (channel1 != channel2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


/* Helper procedures */

/* Convert X to a SSH channel */
struct channel_data *
_scm_to_ssh_channel (SCM x)
{
  scm_assert_smob_type (channel_tag, x);
  return (struct channel_data *) SCM_STREAM (x);
}


/* channel smob initialization. */
void
init_channel_type (void)
{
  channel_tag = scm_make_port_type ("channel",
                                    &ptob_fill_input,
                                    &ptob_write);
  scm_set_port_close (channel_tag, ptob_close);
  scm_set_port_flush (channel_tag, ptob_flush);
  scm_set_port_input_waiting (channel_tag, ptob_input_waiting);
  scm_set_port_mark (channel_tag, mark_channel);
  scm_set_port_free (channel_tag, free_channel);
  scm_set_port_print (channel_tag, print_channel);
  scm_set_port_equalp (channel_tag, equalp_channel);

#include "channel-type.x"
}

/* channel-type.c ends here */
