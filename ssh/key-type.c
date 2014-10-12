/* key-type.c -- SSH key smobs.
 *
 * Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
 *
 * This file is part of Guile-SSH
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

#include "key-type.h"
#include "common.h"

/* BUG: Currently a SSH key that has been read from a file has both
   public and private flags.  It means that we cannot distinguish
   whether the key is private or public by means of
   `ssh_key_is_private' and `ssh_key_is_public' procedures (they both
   return true).

   See `ssh_pki_import_privkey_file' and `pki_private_key_from_base64'
   in libssh 0.6.3 for details. -avp */

scm_t_bits key_tag; /* Smob tag. */

struct symbol_mapping key_types[] = {
  { "dss",     SSH_KEYTYPE_DSS     },
  { "rsa",     SSH_KEYTYPE_RSA     },
  { "rsa1",    SSH_KEYTYPE_RSA1    },
  { "ecdsa",   SSH_KEYTYPE_ECDSA   },
  { "unknown", SSH_KEYTYPE_UNKNOWN },
  { NULL,      -1                  }
};

/* Smob marking */
SCM
mark_key_smob (SCM key_smob)
{
  return SCM_BOOL_F;
}

/* Free the smob. */
size_t
free_key_smob (SCM arg1)
{
  struct key_data *data = _scm_to_key_data (arg1);
  ssh_key_free (data->ssh_key);
  return 0;
}

static int
print_key (SCM smob, SCM port, scm_print_state *pstate)
{
  struct key_data *key_data = _scm_to_key_data (smob);
  SCM type = guile_ssh_key_get_type (smob);

  scm_puts ("#<key ", port);
  scm_display (type, port);
  scm_putc (' ', port);
  scm_puts (_private_key_p (key_data) ? "(private) " : "(public) ", port);
  scm_display (_scm_object_hex_address (smob), port);
  scm_puts (">", port);

  return 1;
}


/* Convert SSH key type to/from a Scheme symbol.
   Possible symbols are: 'dss, 'rsa, 'rsa1, 'ecdsa, 'unknown */

SCM
_ssh_key_type_to_scm (int type)
{
  return _ssh_const_to_scm (key_types, type);
}

struct symbol_mapping *
_scm_to_ssh_key_type (SCM type)
{
  return _scm_to_ssh_const (key_types, type);
}


/* Get the type of the key KEY_SMOB.

   Return a key type as a Scheme symbol.  The type can be one of the
   following list: 'dss, 'rsa, 'rsa1, 'unknown */
SCM_DEFINE (guile_ssh_key_get_type, "get-key-type", 1, 0, 0,
            (SCM key),
            "\
Get a symbol that represents the type of the SSH key KEY.\n\
Possible types are: 'dss, 'rsa, 'rsa1, 'ecdsa, 'unknown\
")
{
  struct key_data *data = _scm_to_key_data (key);
  enum ssh_keytypes_e type = ssh_key_type (data->ssh_key);
  return _ssh_key_type_to_scm (type);
}

SCM_DEFINE (guile_ssh_make_keypair, "make-keypair", 2, 0, 0,
            (SCM type, SCM length),
            "\
Generate a keypair of specified TYPE and LENGTH.  This may take some time.\
Return newly generated private key.  Throw `guile-ssh-error' on error.\
")
#define FUNC_NAME s_guile_ssh_make_keypair
{
  ssh_key key = NULL;
  struct symbol_mapping *c_type = _scm_to_ssh_key_type (type);
  int c_length;
  int res;

  SCM_ASSERT (scm_is_unsigned_integer (length, 9, UINT32_MAX), length,
              SCM_ARG2, FUNC_NAME);

  if (! c_type)
    guile_ssh_error1 (FUNC_NAME, "Wrong key type", type);

  c_length = scm_to_int (length);
  res = ssh_pki_generate (c_type->value, c_length, &key);
  if (res == SSH_ERROR)
    {
      guile_ssh_error1 (FUNC_NAME, "Could not generate key",
                        scm_list_2 (type, length));
    }

  return _scm_from_ssh_key (key);
}
#undef FUNC_NAME


/* Predicates */

SCM_DEFINE (guile_ssh_is_key_p, "key?", 1, 0, 0,
            (SCM x),
            "\
Return #t if X is a SSH key, #f otherwise.\
")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (key_tag, x));
}

SCM_DEFINE (guile_ssh_is_public_key_p, "public-key?", 1, 0, 0,
            (SCM x),
            "\
Return #t if X is a SSH key and it contains a public key, #f otherwise.\
")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (key_tag, x)
                        && _public_key_p (_scm_to_key_data (x)));
}

SCM_DEFINE (guile_ssh_is_private_key_p, "private-key?", 1, 0, 0,
            (SCM x),
            "\
Return #t if X is a SSH private-key, #f otherwise.\
")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (key_tag, x)
                        && _private_key_p (_scm_to_key_data (x)));
}

SCM
equalp_key (SCM x1, SCM x2)
{
  struct key_data *key1 = _scm_to_key_data (x1);
  struct key_data *key2 = _scm_to_key_data (x2);

  if ((! key1) || (! key2))
    return SCM_BOOL_F;
  else if (key1 != key2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


/* Helper procedures */

SCM
_scm_from_ssh_key (ssh_key key)
{
  struct key_data *key_data;
  SCM key_smob;
  key_data = (struct key_data *) scm_gc_malloc (sizeof (struct key_data),
                                                "ssh key");
  key_data->ssh_key = key;
  SCM_NEWSMOB (key_smob, key_tag, key_data);
  return key_smob;
}

/* Convert X to a SSH key */
struct key_data *
_scm_to_key_data (SCM x)
{
  scm_assert_smob_type (key_tag, x);
  return (struct key_data *) SCM_SMOB_DATA (x);
}

/* Check that KEY is a SSH private key. */
inline int
_private_key_p (struct key_data *key)
{
  return ssh_key_is_private (key->ssh_key);
}

/* Check that KEY is a SSH public key */
inline int
_public_key_p (struct key_data *key)
{
  return ssh_key_is_public (key->ssh_key);
}


/* Key smob initialization. */
void
init_key_type (void)
{
  key_tag = scm_make_smob_type ("key", sizeof (struct key_data));
  scm_set_smob_mark (key_tag, mark_key_smob);
  scm_set_smob_free (key_tag, free_key_smob);
  scm_set_smob_print (key_tag, print_key);
  scm_set_smob_equalp (key_tag, equalp_key);

#include "key-type.x"
}

/* private-key.c ends here */
