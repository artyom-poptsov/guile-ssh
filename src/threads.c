/* threads.c -- Initialization of SSH threads
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

#include <libssh/callbacks.h>

static const int PTHREADS_DISABLED = 0;
static const int PTHREADS_ENABLED  = 1;

/* Current SSH threading state */
static int pthreads_state = PTHREADS_DISABLED;

/* Initialize threading if it has not been initialized yet. */
void
init_pthreads (void)
{
  if (pthreads_state == PTHREADS_DISABLED)
    {
      ssh_threads_set_callbacks(ssh_threads_get_pthread());
      ssh_init();
      pthreads_state = PTHREADS_ENABLED;
    }
}

/* threads.c ends here */
