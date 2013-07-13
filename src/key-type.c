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
free_key_smob (SCM key_smob)
{
  struct key_data *data;
  scm_assert_smob_type (key_tag, key_smob);

  data = (struct key_data *) SCM_SMOB_DATA (key_smob);

  switch (data->key_type)
    {
    case KEY_TYPE_NONE:
      ssh_key_free (data->ssh_key);
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
SCM
guile_ssh_key_get_type (SCM key_smob)
{
  struct key_data *data;
  enum ssh_keytypes_e type = SSH_KEYTYPE_UNKNOWN;
  SCM ret;

  scm_assert_smob_type (key_tag, key_smob);

  data = (struct key_data *) SCM_SMOB_DATA (key_smob);

  switch (data->key_type)
    {
    case KEY_TYPE_NONE:
      type = ssh_key_type (data->ssh_key);
      break;

    case KEY_TYPE_PUBLIC:
      type = ssh_key_type (data->ssh_public_key);
      break;

    case KEY_TYPE_PUBLIC_STR:
      type = data->ssh_public_key_str.key_type;
      break;

    case KEY_TYPE_PRIVATE:
      type = ssh_key_type (data->ssh_private_key);
      break;
    }

  return scm_from_ssh_key_type (type);
}


/* Predicates */

SCM
guile_ssh_is_key_p (SCM obj)
{
  return scm_from_bool (SCM_SMOB_PREDICATE (key_tag, obj));
}

SCM
guile_ssh_is_public_key_p (SCM key_smob)
{
  struct key_data *data;

  scm_assert_smob_type (key_tag, key_smob);

  data = (struct key_data *) SCM_SMOB_DATA (key_smob);

  if ((data->key_type == KEY_TYPE_PUBLIC)
      || (data->key_type == KEY_TYPE_PUBLIC_STR))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

SCM
guile_ssh_is_private_key_p (SCM key_smob)
{
  struct key_data *data;

  scm_assert_smob_type (key_tag, key_smob);

  data = (struct key_data *) SCM_SMOB_DATA (key_smob);

  return (data->key_type == KEY_TYPE_PRIVATE) ? SCM_BOOL_T : SCM_BOOL_F;
}


/* Key smob initialization. */
void
init_key_type (void)
{
  key_tag = scm_make_smob_type ("ssh:key", sizeof (struct key_data));
  scm_set_smob_mark (key_tag, mark_key_smob);
  scm_set_smob_free (key_tag, free_key_smob);

  scm_c_define_gsubr ("ssh:key?",         1, 0, 0, guile_ssh_is_key_p);
  scm_c_define_gsubr ("ssh:public-key?",  1, 0, 0, guile_ssh_is_public_key_p);
  scm_c_define_gsubr ("ssh:private-key?", 1, 0, 0, guile_ssh_is_private_key_p);

  scm_c_define_gsubr ("ssh:get-key-type", 1, 0, 0, guile_ssh_key_get_type);
}

/* private-key.c ends here */
