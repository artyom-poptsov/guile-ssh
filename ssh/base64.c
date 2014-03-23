/* base64.c -- Convert base64 (RFC1521) data to a string.
 *
 * These functions are taken with some modifications from base64.c
 * file, that belongs to libssh 0.5.4
 *
 * Original comment:
 */


/*
 * base64.c - support for base64 alphabet system, described in RFC1521
 *
 * This file is part of the SSH Library
 *
 * Copyright (c) 2005-2005 by Aris Adamantiadis
 *
 * The SSH Library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * The SSH Library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with the SSH Library; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 */


/*
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of Guile-SSH.
 *
 * Guile-SSH is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Guile-SSH is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Guile-SSH.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>


static const char ALPHABET[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz"
  "0123456789+/";


static void
_bin_to_base64(unsigned char *dest, 
               const unsigned char source[3],
               size_t len)
{
#define BITS(n) ((1 << (n)) - 1)
  switch (len)
    {
    case 1:
      dest[0] = ALPHABET[(source[0] >> 2)];
      dest[1] = ALPHABET[((source[0] & BITS(2)) << 4)];
      dest[2] = '=';
      dest[3] = '=';
      break;

    case 2:
      dest[0] = ALPHABET[source[0] >> 2];
      dest[1] = ALPHABET[(source[1] >> 4) | ((source[0] & BITS(2)) << 4)];
      dest[2] = ALPHABET[(source[1] & BITS(4)) << 2];
      dest[3] = '=';
      break;

    case 3:
      dest[0] = ALPHABET[(source[0] >> 2)];
      dest[1] = ALPHABET[(source[1] >> 4) | ((source[0] & BITS(2)) << 4)];
      dest[2] = ALPHABET[ (source[2] >> 6) | (source[1] & BITS(4)) << 2];
      dest[3] = ALPHABET[source[2] & BITS(6)];
      break;
    }
#undef BITS
}


/* Converts binary data to a base64 string.

   Return the converted string. */
unsigned char *
bin_to_base64(const unsigned char *source, int len)
{
  unsigned char *base64;
  unsigned char *ptr;
  int flen = len + (3 - (len % 3)); /* round to upper 3 multiple */
  flen = (4 * flen) / 3 + 1;

  base64 = malloc(flen);
  if (base64 == NULL) 
    return NULL;

  ptr = base64;

  while (len > 0)
    {
      _bin_to_base64(ptr, source, (len > 3) ? 3 : len);
      ptr += 4;
      source += 3;
      len -= 3;
    }
  ptr[0] = '\0';

  return base64;
}

/* base64.c ends here */
