/* Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#ifndef __LOG_H__
#define __LOG_H__

#include "common.h"

extern gssh_symbol_t log_verbosity[];

extern void _gssh_log_error (const char* function_name, const char* msg,
                             SCM args);
extern void _gssh_log_error_format (const char* function_name, SCM args,
                                    const char* fmt, ...);

extern void _gssh_log_warning (const char* function_name, const char* msg,
                               SCM args);

#ifdef DEBUG
extern void _gssh_log_debug (const char* function_name, const char* msg,
                             SCM args);
extern void _gssh_log_debug1 (const char* function_name, const char* msg);
extern void _gssh_log_debug_format(const char* function_name, SCM args,
                                   const char* fmt, ...);
#else
#  define _gssh_log_debug(function_name, msg, args)
#  define _gssh_log_debug1(function_name, msg)
#  define _gssh_log_debug_format(function_name, args, fmt, ...)
#endif  /* ifdef DEBUG */

void log_backtrace (const char* function_name);

extern void init_log_func (void);

#endif /* ifndef __LOG_H__ */

/* log.h ends here */
