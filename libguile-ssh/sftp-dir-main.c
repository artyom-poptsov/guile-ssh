/* sftp-dir-main.c -- SFTP dir initialization.
 *
 * Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include "threads.h"
#include "sftp-dir-type.h"
#include "sftp-dir-func.h"

void
init_sftp_dir (void)
{
    init_sftp_dir_type ();
    init_sftp_dir_func ();
    init_pthreads ();
}

/* sftp-dir-main.c ends here. */
