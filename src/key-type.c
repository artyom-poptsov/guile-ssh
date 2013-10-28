/* key-type.c -- SSH key smobs.
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

#include "key-type.h"

scm_t_bits key_tag; /* Smob tag. */

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
  struct key_data *data = _scm_to_ssh_key (arg1);

  switch (data->key_type)
    {
    case KEY_TYPE_NONE:
      publickey_free ((ssh_public_key) data->ssh_key);
      break;

    case KEY_TYPE_PUBLIC:
      publickey_free (data->ssh_public_key);
      break;

    case KEY_TYPE_PUBLIC_STR:
      ssh_string_free (data->ssh_public_key_str.key);
      break;

    case KEY_TYPE_PRIVATE:
      privatekey_free (data->ssh_private_key);
      break;
    }

  return 0;
}

static int
print_key (SCM smob, SCM port, scm_print_state *pstate)
{
  struct key_data *key_data = _scm_to_ssh_key (smob);
  SCM type = guile_ssh_key_get_type (smob);

  scm_puts ("#<", port);
  scm_puts (_public_key_p (key_data) ? "public" : "private", port);
  scm_puts (" ", port);
  scm_display (type, port);
  scm_puts (" key", port);
  scm_puts (">", port);

  return 1;
}


/* Convert SSH key type to a Scheme symbol.

   Return a key type as a Scheme symbol.  The type can be one of the
   following list: 'dss, 'rsa, 'rsa1, 'unknown */
static SCM
scm_from_ssh_key_type (int type)
{
  switch (type)
    {
    case SSH_KEYTYPE_DSS:
      return scm_from_locale_symbol ("dss");

    case SSH_KEYTYPE_RSA:
      return scm_from_locale_symbol ("rsa");      

    case SSH_KEYTYPE_RSA1:
      return scm_from_locale_symbol ("rsa1");

    case SSH_KEYTYPE_UNKNOWN:
    default:
      return scm_from_locale_symbol ("unknown");
    }
}

/* Get the type of the key KEY_SMOB.

   Return a key type as a Scheme symbol.  The type can be one of the
   following list: 'dss, 'rsa, 'rsa1, 'unknown */
SCM_DEFINE (guile_ssh_key_get_type, "get-key-type", 1, 0, 0,
            (SCM key),
            "Get a symbol that represents the type of the SSH key KEY.\n"
            "Possible types are: 'dss, 'rsa, 'rsa1, 'unknown")
{
  struct key_data *data = _scm_to_ssh_key (key);
  enum ssh_keytypes_e type = SSH_KEYTYPE_UNKNOWN;

  switch (data->key_type)
    {
    case KEY_TYPE_NONE:
      type = ssh_privatekey_type ((ssh_private_key) data->ssh_key);
      break;

    case KEY_TYPE_PUBLIC:
      type = ssh_privatekey_type ((ssh_private_key) data->ssh_public_key);
      break;

    case KEY_TYPE_PUBLIC_STR:
      type = data->ssh_public_key_str.key_type;
      break;

    case KEY_TYPE_PRIVATE:
      type = ssh_privatekey_type (data->ssh_private_key);
      break;
    }

  return scm_from_ssh_key_type (type);
}


/* Predicates */

SCM_DEFINE (guile_ssh_is_key_p, "key?", 1, 0, 0,
            (SCM x),
            "Return #t if X is a SSH key, #f otherwise.")
{
  return scm_from_bool (SCM_SMOB_PREDICATE (key_tag, x));
}

SCM_DEFINE (guile_ssh_is_public_key_p, "public-key?", 1, 0, 0,
            (SCM x),
            "Return #t if X is a SSH public-key, #f otherwise.")
{
  struct key_data *key = _scm_to_ssh_key (x);
  return scm_from_bool (_public_key_p (key));
}

SCM_DEFINE (guile_ssh_is_private_key_p, "private-key?", 1, 0, 0,
            (SCM x),
            "Return #t if X is a SSH private-key, #f otherwise.")
{
  struct key_data *key = _scm_to_ssh_key (x);
  return scm_from_bool (_private_key_p (key));
}

SCM
equalp_key (SCM x1, SCM x2)
{
  struct key_data *key1 = _scm_to_ssh_key (x1);
  struct key_data *key2 = _scm_to_ssh_key (x2);

  if ((! key1) || (! key2))
    return SCM_BOOL_F;
  else if (key1 != key2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


/* Helper procedures */

/* Convert X to a SSH key */
struct key_data *
_scm_to_ssh_key (SCM x)
{
  scm_assert_smob_type (key_tag, x);
  return (struct key_data *) SCM_SMOB_DATA (x);
}

/* Check that KEY is a SSH private key. */
inline int
_private_key_p (struct key_data *key)
{
  return (key->key_type == KEY_TYPE_PRIVATE);
}

/* Check that KEY is a SSH public key */
inline int
_public_key_p (struct key_data *key)
{
  return ((key->key_type == KEY_TYPE_PUBLIC)
          || (key->key_type == KEY_TYPE_PUBLIC_STR));
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
