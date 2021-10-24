/* channel-type.c -- SSH channel smob.
 *
 * Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 * Copyright (C) 2017 Ludovic Courtès <ludo@gnu.org>
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

#include <config.h>

#include <libguile.h>
#include <libssh/libssh.h>
#include <libssh/callbacks.h>
#include <assert.h>

#include "session-type.h"
#include "channel-type.h"
#include "error.h"
#include "common.h"
#include "log.h"

static const char* GSSH_CHANNEL_TYPE_NAME = "channel";


gssh_port_t channel_tag;

enum {
  DEFAULT_PORT_R_BUFSZ = 256,      /* Default read buffer size */
  DEFAULT_PORT_W_BUFSZ = 1         /* Default write buffer size */
};


/* Ptob specific procedures */

#if USING_GUILE_BEFORE_2_2

/* Read data from the channel.  Return EOF if no data is available or
   throw `guile-ssh-error' if an error occurred. */
static int
ptob_fill_input (SCM channel)
#define FUNC_NAME "ptob_fill_input"
{
  gssh_channel_t *cd = gssh_channel_from_scm (channel);
  scm_port *pt = SCM_PTAB_ENTRY (channel);
  int res;

  if (cd->is_remote_closed
      || (! _gssh_channel_parent_session_connected_p (cd)))
    {
      return EOF;
    }

  if (! ssh_channel_is_open (cd->ssh_channel))
    return EOF;

  /* Update state of the underlying channel and check whether we have
     data to read or not. */
  res = ssh_channel_poll_timeout (cd->ssh_channel, cd->timeout, cd->is_stderr);
  switch (res) {
  case SSH_ERROR:
    guile_ssh_error1 (FUNC_NAME, "Error polling channel", channel);
  case SSH_EOF:
    return EOF;
  case 0:
    log_backtrace (FUNC_NAME);
    assert (0);
    return EOF;                 /* Must not happen. */
  }

  else if (res == SSH_EOF)
    return EOF;

  res = ssh_channel_read (cd->ssh_channel,
                          pt->read_buf, pt->read_buf_size,
                          cd->is_stderr);

  if (res == SSH_ERROR)
    guile_ssh_error1 (FUNC_NAME, "Error reading from the channel", channel);

  /* `ssh_channel_read' sometimes returns 0 even if `ssh_channel_poll' returns
     a positive value.  So we must ensure that res != 0 otherwise an assertion
     in `scm_i_fill_input' won't be meet (see `ports.c' in Guile 2.0.9). */
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
  gssh_channel_t *channel_data = gssh_channel_from_scm (channel);
  int res = ssh_channel_write (channel_data->ssh_channel, data, sz);
  if (res == SSH_ERROR)
    {
      ssh_session session = ssh_channel_get_session (channel_data->ssh_channel);
      guile_ssh_session_error1 (FUNC_NAME, session, channel);
    }

  if (res < sz)
    scm_syserror (FUNC_NAME);
}
#undef FUNC_NAME

/* Complete the processing of buffered output data.  Currently this
   callback makes no effect because the channel CHANNEL uses
   unbuffered output. */
static void
ptob_flush (SCM channel)
#define FUNC_NAME "ptob_flush"
{
  scm_port *pt = SCM_PTAB_ENTRY (channel);
  gssh_channel_t *cd = gssh_channel_from_scm (channel);
  size_t wrsize = pt->write_pos - pt->write_buf;

  if (wrsize)
    {
      int res = ssh_channel_write (cd->ssh_channel, pt->write_buf, wrsize);
      if (res == SSH_ERROR)
        {
          ssh_session session = ssh_channel_get_session (cd->ssh_channel);
          guile_ssh_session_error1 (FUNC_NAME, session, channel);
        }
    }

  pt->write_pos = pt->write_buf;
}
#undef FUNC_NAME

#else /* !USING_GUILE_BEFORE_2_2 */

static size_t
read_from_channel_port (SCM channel, SCM dst, size_t start, size_t count)
#define FUNC_NAME "read_from_channel_port"
{
  char *data = (char *) SCM_BYTEVECTOR_CONTENTS (dst) + start;
  gssh_channel_t *cd = gssh_channel_from_scm (channel);
  int res;

  if (! ssh_channel_is_open (cd->ssh_channel))
    return 0;

  /* Update state of the underlying channel and check whether we have
     data to read or not. */
  res = ssh_channel_poll (cd->ssh_channel, cd->is_stderr);
  if (res == SSH_ERROR)
    {
      if (cd->is_remote_closed
          || (! _gssh_channel_parent_session_connected_p (cd)))
        {
          return 0;
        }
      else
        {
          guile_ssh_error1 (FUNC_NAME,
                            "Error polling channel",
                            channel);
        }
    }

  else if (res == SSH_EOF)
    return 0;

  /* Note: `ssh_channel_read' sometimes returns 0 even if `ssh_channel_poll'
     returns a positive value.  */
  res = ssh_channel_read (cd->ssh_channel, data, count, cd->is_stderr);

  if (res == SSH_AGAIN)
    {
      res = 0;
    }
  else if (res == SSH_ERROR)
    {
      if (cd->is_remote_closed
          || (! _gssh_channel_parent_session_connected_p (cd)))
        {
          res = (scm_t_wchar) 0;
        }
      else
        {
          guile_ssh_error1 (FUNC_NAME,
                            "Error reading from the channel",
                            channel);
        }
    }

  return res;
}
#undef FUNC_NAME

static size_t
write_to_channel_port (SCM channel, SCM src, size_t start, size_t count)
#define FUNC_NAME "write_to_channel_port"
{
  char *data = (char *) SCM_BYTEVECTOR_CONTENTS (src) + start;
  gssh_channel_t *channel_data = gssh_channel_from_scm (channel);

  if (channel_data->is_remote_closed)
    return 0;

  if (! _gssh_channel_parent_session_connected_p (channel_data))
    guile_ssh_error1 (FUNC_NAME, "Parent session is not connected", channel);

  int res = ssh_channel_write (channel_data->ssh_channel, data, count);
  if (res == SSH_ERROR)
    {
      ssh_session session = ssh_channel_get_session (channel_data->ssh_channel);
      guile_ssh_session_error1 (FUNC_NAME, session, channel);
    }

  return res;
}
#undef FUNC_NAME

#endif /* !USING_GUILE_BEFORE_2_2 */

/* Poll the underlying SSH channel for data, return amount of data
   available for reading.  Throw `guile-ssh-error' on error. */
static int
ptob_input_waiting (SCM channel)
#define FUNC_NAME "ptob_input_waiting"
{
  gssh_channel_t *cd = gssh_channel_from_scm (channel);
  int res = ssh_channel_poll (cd->ssh_channel, cd->is_stderr);

  if (res == SSH_ERROR)
    guile_ssh_error1 (FUNC_NAME, "An error occurred.", channel);

  return (res != SSH_EOF) ? res : 0;
}
#undef FUNC_NAME

/* Close underlying SSH channel and free all allocated resources. */
#if USING_GUILE_BEFORE_2_2
static int
#else
static void
#endif
ptob_close (SCM channel)
{
  gssh_channel_t *ch = gssh_channel_from_scm (channel);

#if USING_GUILE_BEFORE_2_2
  scm_port *pt = SCM_PTAB_ENTRY (channel);

  ptob_flush (channel);
#endif

  if (ch)
    {
      gssh_session_t *sd = gssh_session_from_scm (ch->session);
      gssh_session_del_channel_x (sd, channel);
      if (ch->is_remote_closed == 1)
        {
          _gssh_log_debug1 ("ptob_close",
                            "the channel is already freed"
                            " by the closing request from the remote side.");
        }
      else if (sd && ssh_is_connected (sd->ssh_session))
        {
          if (ssh_channel_is_open (ch->ssh_channel))
            {
              _gssh_log_debug ("ptob_close", "closing and freeing the channel...",
                               channel);
              ssh_channel_free (ch->ssh_channel);
              _gssh_log_debug1 ("ptob_close", "closing and freeing the channel... done");
            }
        }
      else
        {
          _gssh_log_debug1 ("ptob_close",
                            "the channel is already freed"
                            " along with the parent session.");
        }
      free (ch->callbacks);
      ch->callbacks = NULL;
      scm_gc_unprotect_object (ch->session);
    }
  else
    {
      _gssh_log_debug1 ("ptob_close", "the channel is already freed.");
    }

  SCM_SETSTREAM (channel, NULL);

#if USING_GUILE_BEFORE_2_2
  scm_gc_free (pt->write_buf, pt->write_buf_size, "port write buffer");
  scm_gc_free (pt->read_buf,  pt->read_buf_size, "port read buffer");

  return 0;
#endif
}

/* Print the CHANNEL object to port PORT. */
static int
print_channel (SCM channel, SCM port, scm_print_state *pstate)
{
  gssh_channel_t *ch = NULL;

#if USING_GUILE_BEFORE_2_2
  if (SCM_PTAB_ENTRY (channel))
    ch = gssh_channel_from_scm (channel);
#else
  ch = gssh_channel_from_scm (channel);
#endif

  scm_puts ("#<", port);

  if (! ch)
    {
      scm_puts ("unknown channel (freed) ", port);
    }
  else
    {
      if (! _gssh_channel_parent_session_connected_p (ch))
        {
          scm_puts ("unknown channel (freed) ", port);
        }
      else
        {
          scm_print_port_mode (channel, port);
          scm_puts ("channel ", port);
          if (SCM_OPPORTP (channel))
            {
              if (ssh_channel_is_open (ch->ssh_channel))
                {
                  scm_puts ("(open) ", port);
                }
              else if (ch->is_remote_closed == 1)
                {
                  scm_puts ("(closed by the remote side) ", port);
                }
              else
                {
                  scm_puts ("(closed) ", port);
                }
            }
          else
            {
              scm_puts ("(closed) ", port);
            }
        }
    }
  scm_display (_scm_object_hex_address (channel), port);
  scm_puts (">", port);
  return 1;
}

static void
channel_close_callback (ssh_session session, ssh_channel channel, void *userdata)
{
  SCM scm_channel = (SCM) userdata;
  gssh_channel_t* cd = gssh_channel_from_scm (scm_channel);
  cd->is_remote_closed = 1;
}


/* Helper procedures */

/* Pack the SSH channel CH to a Scheme port and return newly created
   port.

   Asserts:
   - FLAGS variable has only SCM_RDNG and SCM_WRTNG bits set.
   */
SCM
ssh_channel_to_scm (ssh_channel ch, SCM session, long flags)
{
  SCM ptob;
  gssh_channel_t *channel_data;

  assert ((flags & ~(SCM_RDNG | SCM_WRTNG)) == 0);

  channel_data = scm_gc_malloc (sizeof (gssh_channel_t),
                                GSSH_CHANNEL_TYPE_NAME);

  channel_data->ssh_channel = ch;
  channel_data->is_stderr = 0;  /* Reading from stderr disabled by default */
  channel_data->session = session;
  channel_data->is_remote_closed = 0;
  channel_data->timeout = -1;   /* Infinite timeout. */

  scm_gc_protect_object (channel_data->session);

#if USING_GUILE_BEFORE_2_2
  {
    scm_port *pt;

    ptob = scm_new_port_table_entry (channel_tag);
    pt   = SCM_PTAB_ENTRY (ptob);

    pt->rw_random = 0;

    /* Output init */
    pt->write_buf_size = DEFAULT_PORT_W_BUFSZ;
    pt->write_buf = scm_gc_malloc (pt->write_buf_size, "port write buffer");
    pt->write_pos = pt->write_buf;
    pt->write_end = pt->write_buf;

    /* Input init */
    pt->read_buf_size = DEFAULT_PORT_R_BUFSZ;
    pt->read_buf = scm_gc_malloc (pt->read_buf_size, "port read buffer");
    pt->read_pos = pt->read_buf;
    pt->read_end = pt->read_buf;

    SCM_SET_CELL_TYPE (ptob, channel_tag | flags);
    SCM_SETSTREAM (ptob, channel_data);
  }
#else
  /* As for file ports returned by 'socket', 'accept', & co., make the port
     unbuffered by default so that writes go straight to the remote host, as
     people typically expect.  */
  ptob = scm_c_make_port (channel_tag, flags | SCM_BUF0,
			  (scm_t_bits) channel_data);
#endif

  channel_data->callbacks
    = calloc (1, sizeof (struct ssh_channel_callbacks_struct));

  channel_data->callbacks->channel_close_function = channel_close_callback;
  channel_data->callbacks->userdata               = (void *) ptob;
  ssh_callbacks_init (channel_data->callbacks);

  if (ssh_set_channel_callbacks (ch, channel_data->callbacks) != SSH_OK)
    {
      guile_ssh_error1(__func__, "Could not set channel callbacks", ptob);
    }

  return ptob;
}

/* Convert X to a SSH channel.  Return the channel data or NULL if the channel
   has been freed. */
gssh_channel_t *
gssh_channel_from_scm (SCM x)
{
  /* In Guile 2.0 ports and SMOBs were all alike; that is no longer the case
     in 2.2.  */
#if USING_GUILE_BEFORE_2_2
  scm_assert_smob_type (channel_tag, x);
#else
  SCM_ASSERT_TYPE (SCM_PORTP (x) && SCM_PORT_TYPE (x) == channel_tag,
		   x, 1, __func__, "channel-port");
#endif

  return (gssh_channel_t *) SCM_STREAM (x);
}

/**
 * Predicate. Return 1 if the parent session is connected, 0 otherwise.
 */
int
_gssh_channel_parent_session_connected_p (gssh_channel_t* cd)
{
  gssh_session_t *sd = gssh_session_from_scm (cd->session);
  return (sd && ssh_is_connected (sd->ssh_session));
}


/* channel smob initialization. */
void
init_channel_type (void)
{
  channel_tag = scm_make_port_type ((char*) GSSH_CHANNEL_TYPE_NAME,
#if USING_GUILE_BEFORE_2_2
                                    &ptob_fill_input,
                                    &ptob_write
#else
				    read_from_channel_port,
				    write_to_channel_port
#endif
				    );
  scm_set_port_close (channel_tag, ptob_close);
  scm_set_port_needs_close_on_gc (channel_tag, 1);

#if USING_GUILE_BEFORE_2_2
  scm_set_port_flush (channel_tag, ptob_flush);

  /* The 'equalp' function has no equivalent with Guile 2.2 but 'eq?' should
     be equivalent in practice.  */
  scm_set_port_equalp (channel_tag, equalp_channel);
#endif

  scm_set_port_input_waiting (channel_tag, ptob_input_waiting);
  scm_set_port_print (channel_tag, print_channel);

  scm_c_define ("RDNG",  scm_from_long (SCM_RDNG));
  scm_c_define ("WRTNG", scm_from_long (SCM_WRTNG));

#include "channel-type.x"
}

/* channel-type.c ends here */
