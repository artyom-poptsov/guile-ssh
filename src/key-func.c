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
/* Debug */
#include <assert.h>
#include <string.h>
#include <arpa/inet.h>

#include "key-type.h"
#include "session-type.h"

/* Convert SSH public key to a scheme string. 
 *
 * TODO: Probably should be replaced with a simply print function.
 */
SCM
guile_ssh_public_key_to_string (SCM key_smob)
{
  struct key_data *data;
  ssh_string str_key;
  SCM ret;

  SCM_ASSERT (scm_to_bool (guile_ssh_is_public_key_p (key_smob)),
	      key_smob, SCM_ARG4, __func__);

  data = (struct key_data *) SCM_SMOB_DATA (key_smob);

  /* TODO: Are there any memory leaks because of conversion between
           data types? */
  str_key = publickey_to_string (data->ssh_public_key);
  ret = scm_from_locale_string (ssh_string_to_char (str_key));
  ssh_string_free (str_key);

  return ret;
}

/* Read private key from a file FILENAME */
SCM
guile_ssh_private_key_from_file (SCM session_smob, SCM filename)
{
  SCM key_smob;
  struct session_data *session_data;
  struct key_data *key_data;
  char *c_filename;

  char *passphrase = NULL;	/* No passphrase TODO: Fix it. */

  scm_dynwind_begin (0);

  scm_assert_smob_type (session_tag, session_smob);
  SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG2, __func__);

  session_data = (struct session_data *) SCM_SMOB_DATA (session_smob);
  key_data = (struct key_data *) scm_gc_malloc (sizeof (struct key_data),
						"ssh key");

  c_filename = scm_to_locale_string (filename);
  scm_dynwind_free (c_filename);

  key_data->key_type = KEY_TYPE_PRIVATE;
  key_data->ssh_private_key = privatekey_from_file (session_data->ssh_session,
						    c_filename,
						    0,	/* Detect key type automatically */
						    passphrase);
  if (key_data->ssh_private_key == NULL)
    {
      scm_display (scm_from_locale_string (c_filename), scm_current_output_port ());
      ssh_error (__func__, ssh_get_error (session_data->ssh_session),
		 SCM_BOOL_F, SCM_BOOL_F);
    }

  SCM_NEWSMOB (key_smob, key_tag, key_data);

  scm_dynwind_end ();

  return key_smob;
}

/* Get public key from a private key KEY_SMOB */
SCM
guile_ssh_public_key_from_private_key (SCM key_smob)
{
  struct key_data *private_key_data;
  struct key_data *public_key_data;
  SCM smob;

  SCM_ASSERT (scm_to_bool (guile_ssh_is_private_key_p (key_smob)),
	      key_smob, SCM_ARG4, __func__);

  private_key_data = (struct key_data *) SCM_SMOB_DATA (key_smob);

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

/* Read public key from a file FILENAME 
 *
 * FIXME: It doesn't work properly because of strange behaviour of
 *        ssh_string_to_char :-/
 */
SCM
guile_ssh_public_key_from_file (SCM session_smob, SCM filename)
{
  struct session_data *session_data;
  char *c_filename;
  ssh_string public_key_str;
  SCM public_key;

  scm_dynwind_begin (0);

  scm_assert_smob_type (session_tag, session_smob);
  SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG2, __func__);

  session_data = (struct session_data *) SCM_SMOB_DATA (session_smob);

  c_filename = scm_to_locale_string (filename);
  scm_dynwind_free (c_filename);

  public_key_str = publickey_from_file (session_data->ssh_session,
  					c_filename,
  					NULL);	/* Detect key type automatically */

  if (public_key_str == NULL)
    return SCM_BOOL_F;

  public_key = scm_from_locale_string (ssh_string_to_char (public_key_str));

  scm_dynwind_end ();

  return public_key;
}

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
