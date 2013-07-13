/* key-func.c -- SSH key manipulation functions.
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
#include "session-type.h"
#include "base64.h"

/* Convert a public key to SSH string.  Return newly allocated SSH
   string.  NOTE that a) the string should be freed after usage, and
   b) the function doesn't do any checks for the key type. */
inline ssh_string
public_key_to_ssh_string (const struct key_data *public_key_data)
{
  if (public_key_data->key_type == KEY_TYPE_PUBLIC)
    return publickey_to_string (public_key_data->ssh_public_key);
  else                          /* key_type == KEY_TYPE_PUBLIC_STR */
    return ssh_string_copy (public_key_data->ssh_public_key_str.key);
}

/* Convert SSH public key to a scheme string.
 *
 * TODO: Probably should be replaced with a simply print function.
 */
SCM
guile_ssh_public_key_to_string (SCM arg1)
{
  struct key_data *key_data = _scm_to_ssh_key (arg1);
  ssh_string public_key;
  unsigned char *key_str;
  size_t        key_len;
  SCM ret;

  scm_dynwind_begin (0);

  SCM_ASSERT (_public_key_p (key_data), arg1, SCM_ARG1, __func__);

  public_key = public_key_to_ssh_string (key_data);
  scm_dynwind_unwind_handler ((void (*)(void*)) ssh_string_free, public_key,
                              SCM_F_WIND_EXPLICITLY);

  key_str = (unsigned char *) ssh_string_to_char (public_key);
  scm_dynwind_free (key_str);

  key_len = ssh_string_len (public_key);

  /* Convert the public key from binary representation to a base64. */
  ret = scm_from_locale_string (bin_to_base64 (key_str, key_len));

  scm_dynwind_end ();

  return ret;
}

/* Read private key from a file FILENAME */
SCM
guile_ssh_private_key_from_file (SCM session_smob, SCM filename)
{
  SCM key_smob;
  struct session_data *session_data = _scm_to_ssh_session (session_smob);
  struct key_data *key_data;
  char *c_filename;

  char *passphrase = NULL;      /* No passphrase TODO: Fix it. */

  scm_dynwind_begin (0);

  SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG2, __func__);

  key_data = (struct key_data *) scm_gc_malloc (sizeof (struct key_data),
                                                "ssh key");

  c_filename = scm_to_locale_string (filename);
  scm_dynwind_free (c_filename);

  key_data->key_type = KEY_TYPE_PRIVATE;
  key_data->ssh_private_key = privatekey_from_file (session_data->ssh_session,
                                                    c_filename,
                                                    0, /* Detect key type
                                                          automatically */
                                                    passphrase);

  if (key_data->ssh_private_key == NULL)
    return SCM_BOOL_F;;

  SCM_NEWSMOB (key_smob, key_tag, key_data);

  scm_dynwind_end ();

  return key_smob;
}

/* Get public key from a private key KEY_SMOB */
SCM
guile_ssh_public_key_from_private_key (SCM arg1)
{
  struct key_data *private_key_data = _scm_to_ssh_key (arg1);
  struct key_data *public_key_data;
  SCM smob;

  SCM_ASSERT (_private_key_p (private_key_data), arg1, SCM_ARG1, __func__);

  public_key_data = (struct key_data *) scm_gc_malloc (sizeof (struct key_data),
                                                       "ssh key");

  public_key_data->key_type = KEY_TYPE_PUBLIC;

  public_key_data->ssh_public_key 
    = publickey_from_privatekey (private_key_data->ssh_private_key);

  if (public_key_data->ssh_public_key == NULL)
    return SCM_BOOL_F;

  SCM_NEWSMOB (smob, key_tag, public_key_data);

  return smob;
}

/* Read public key from a file FILENAME.
 *
 * Return a SSH key smob.
 */
SCM
guile_ssh_public_key_from_file (SCM session_smob, SCM filename)
{
  struct session_data *session_data = _scm_to_ssh_session (session_smob);
  struct key_data *public_key_data;
  char *c_filename;
  ssh_string public_key_str;
  SCM key_smob;
  int key_type;

  scm_dynwind_begin (0);

  SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG2, __func__);

  c_filename = scm_to_locale_string (filename);
  scm_dynwind_free (c_filename);

  public_key_str = publickey_from_file (session_data->ssh_session,
                                        c_filename,
                                        &key_type);

  if (public_key_str == NULL)
      return SCM_BOOL_F;

  public_key_data = (struct key_data *) scm_gc_malloc (sizeof (struct key_data),
                                                       "ssh key");
  public_key_data->key_type = KEY_TYPE_PUBLIC_STR;
  public_key_data->ssh_public_key_str.key      = public_key_str;
  public_key_data->ssh_public_key_str.key_type = key_type;

  SCM_NEWSMOB (key_smob, key_tag, public_key_data);

  scm_dynwind_end ();

  return key_smob;
}


/* Initialize Scheme procedures. */
void
init_key_func (void)
{
  scm_c_define_gsubr ("ssh:public-key->string", 1, 0, 0,
                      guile_ssh_public_key_to_string);
  scm_c_define_gsubr ("ssh:public-key-from-file", 2, 0, 0,
                      guile_ssh_public_key_from_file);
  scm_c_define_gsubr ("ssh:private-key->public-key", 1, 0, 0,
                      guile_ssh_public_key_from_private_key);
  scm_c_define_gsubr ("ssh:private-key-from-file", 2, 0, 0,
                      guile_ssh_private_key_from_file);
}

/* key-func.c ends here */
