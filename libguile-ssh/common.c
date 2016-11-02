/* common.c -- Common functions.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <libguile.h>
#include <libssh/libssh.h>
#include "common.h"

/* Convert the SSH constant VALUE to a Scheme symbol */
SCM
_ssh_const_to_scm (const struct symbol_mapping *types, int value)
{
  const struct symbol_mapping *t;
  for (t = types; t->symbol; ++t)
    {
      if (t->value == value)
        return scm_from_locale_symbol (t->symbol);
    }
  return SCM_BOOL_F;
}

/* Convert the Scheme symbol VALUE to a SSH constant.

   Return the apropriate structure that contains the needed
   constant. */
const struct symbol_mapping *
_scm_to_ssh_const (const struct symbol_mapping *types, SCM value)
{
  const struct symbol_mapping *t;
  char *sym = scm_to_locale_string (scm_symbol_to_string (value));
  for (t = types; t->symbol; ++t)
    {
      if (! strcmp (t->symbol, sym))
        return t;
    }
  return NULL;
}


/* Return an address of the object OBJ as an hexadecimal number represented as
   a string. */
SCM
_scm_object_hex_address (SCM obj)
{
  return scm_number_to_string (scm_object_address (obj), scm_from_uint (16U));
}

/* common.c ends here. */
