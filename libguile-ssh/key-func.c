/* key-func.c -- SSH key manipulation functions.
 *
 * Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <libguile.h>
#include <libssh/libssh.h>

#include "key-type.h"
#include "common.h"
#include "error.h"

/* Convert SSH public key KEY to a scheme string. */
SCM_DEFINE (guile_ssh_public_key_to_string, "public-key->string", 1, 0, 0,
            (SCM key),
            "\
Convert SSH public key to a scheme string.\
")
#define FUNC_NAME s_guile_ssh_public_key_to_string
{
  gssh_key_t *key_data = _scm_to_key_data (key);
  char *key_str;

  SCM_ASSERT (_public_key_p (key_data), key, SCM_ARG1, FUNC_NAME);

  int res = ssh_pki_export_pubkey_base64 (key_data->ssh_key, &key_str);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Unable to convert the key to a string", key);

  return scm_take_locale_string (key_str);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_string_to_public_key, "string->public-key", 2, 0, 0,
            (SCM base64_str, SCM type),
            "\
Convert Base64 string to a public key.  Return new public key.\n\
Throw `guile-ssh-error' on error.\
")
#define FUNC_NAME s_guile_ssh_string_to_public_key
{
  char *c_base64_str = NULL;
  const struct symbol_mapping *key_type = NULL;
  ssh_key ssh_public_key = NULL;
  int res;

  SCM_ASSERT (scm_is_string (base64_str), base64_str, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_symbol (type),       type,   SCM_ARG2, FUNC_NAME);

  scm_dynwind_begin (0);

  c_base64_str = scm_to_locale_string (base64_str);
  scm_dynwind_free (c_base64_str);

  key_type = _scm_to_ssh_key_type (type);
  if (! key_type)
    guile_ssh_error1 (FUNC_NAME, "Wrong key type", type);

  res = ssh_pki_import_pubkey_base64 (c_base64_str,
                                      key_type->value,
                                      &ssh_public_key);
  if (res != SSH_OK)
    {
      const char *msg = "Could not convert the given string to a public key";
      guile_ssh_error1 (FUNC_NAME, msg, scm_list_2 (base64_str, type));
    }

  scm_dynwind_end ();

  return _scm_from_ssh_key (ssh_public_key, SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_private_key_from_file, "private-key-from-file", 1, 0, 0,
            (SCM filename),
            "\
Read private key from a file FILENAME.  If the the key isn encrypted the user\n\
will be asked for passphrase to decrypt the key.\n\
\n\
Return a new SSH key of #f on error.\
")
#define FUNC_NAME s_guile_ssh_private_key_from_file
{
  ssh_key ssh_key = NULL;
  char *c_filename;
  /* NULL means that either the public key is unecrypted or the user
     should be asked for the passphrase. */
  char *passphrase = NULL;
  int res;

  scm_dynwind_begin (0);

  SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG1, FUNC_NAME);

  c_filename = scm_to_locale_string (filename);
  scm_dynwind_free (c_filename);

  res = ssh_pki_import_privkey_file (c_filename,
                                     passphrase,
                                     NULL, /* auth_fn */
                                     NULL, /* auth_data */
                                     &ssh_key);

  if (res == SSH_EOF)
    {
      const char *msg = "The file does not exist or permission denied";
      guile_ssh_error1 (FUNC_NAME, msg, filename);
    }
  else if (res == SSH_ERROR)
    {
      const char *msg = "Unable to import a key from the file";
      guile_ssh_error1 (FUNC_NAME, msg, filename);
    }

  scm_dynwind_end ();

  return _scm_from_ssh_key (ssh_key, SCM_BOOL_F);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_private_key_to_file,
            "private-key-to-file", 2, 0, 0,
            (SCM key, SCM file_name),
            "\
Export a private KEY to file FILE_NAME.  Throw `guile-ssh-error' on error. \
Return value is undefined.\
")
#define FUNC_NAME s_guile_ssh_private_key_to_file
{
  gssh_key_t *kd = _scm_to_key_data (key);
  char *c_file_name = NULL;
  int res;

  scm_dynwind_begin (0);

  SCM_ASSERT (_private_key_p (kd),       key,       SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_string (file_name), file_name, SCM_ARG2, FUNC_NAME);

  c_file_name = scm_to_locale_string (file_name);
  scm_dynwind_free (c_file_name);

  res = ssh_pki_export_privkey_file (kd->ssh_key,
                                     NULL, /* passphrase */
                                     NULL, /* auth_fn */
                                     NULL, /* auth_data */
                                     c_file_name);
  if (res == SSH_ERROR)
    {
      guile_ssh_error1 (FUNC_NAME, "Unable to export a key to a file",
                        scm_list_2 (key, file_name));
    }

  scm_dynwind_end ();

  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_public_key_from_private_key, "private-key->public-key",
            1, 0, 0,
            (SCM key),
            "\
Get public key from a private key KEY.\
")
#define FUNC_NAME s_guile_ssh_public_key_from_private_key
{
  gssh_key_t *private_key_data = _scm_to_key_data (key);
  ssh_key ssh_public_key = NULL;
  int res;

  SCM_ASSERT (_private_key_p (private_key_data), key, SCM_ARG1, FUNC_NAME);

  res = ssh_pki_export_privkey_to_pubkey (private_key_data->ssh_key,
                                          &ssh_public_key);

  if (res != SSH_OK)
    return SCM_BOOL_F;

  return _scm_from_ssh_key (ssh_public_key, SCM_BOOL_F);
}
#undef FUNC_NAME

/* Read public key from a file FILENAME.
 *
 * Return a SSH key smob.
 */
SCM_DEFINE (guile_ssh_public_key_from_file, "public-key-from-file", 1, 0, 0,
            (SCM filename),
            "\
Read public key from a file FILENAME.  Return a SSH key.\
")
#define FUNC_NAME s_guile_ssh_public_key_from_file
{
  ssh_key ssh_public_key = NULL;
  char *c_filename;
  int res;

  scm_dynwind_begin (0);

  SCM_ASSERT (scm_is_string (filename), filename, SCM_ARG1, FUNC_NAME);

  c_filename = scm_to_locale_string (filename);
  scm_dynwind_free (c_filename);

  res = ssh_pki_import_pubkey_file (c_filename, &ssh_public_key);

  if (res == SSH_EOF)
    {
      const char *msg = "The file does not exist or permission denied";
      guile_ssh_error1 (FUNC_NAME, msg, filename);
    }
  else if (res == SSH_ERROR)
    {
      const char *msg = "Unable to import a key from the file";
      guile_ssh_error1 (FUNC_NAME, msg, filename);
    }

  scm_dynwind_end ();

  return _scm_from_ssh_key (ssh_public_key, SCM_BOOL_F);
}
#undef FUNC_NAME

static struct symbol_mapping hash_types[] = {
  { "sha1", SSH_PUBLICKEY_HASH_SHA1 },
  { "md5",  SSH_PUBLICKEY_HASH_MD5  },
  { NULL,   -1                      }
};

SCM_DEFINE (guile_ssh_get_public_key_hash, "get-public-key-hash", 2, 0, 0,
            (SCM key, SCM type),
            "\
Get hash of the public KEY as a bytevector.\n\
Possible types are: 'sha1, 'md5\n\
Return a bytevector on success, #f on error.\
")
#define FUNC_NAME s_guile_ssh_get_public_key_hash
{
  gssh_key_t *kd = _scm_to_key_data (key);
  unsigned char *hash = NULL;
  size_t hash_len;
  int res;
  SCM ret;
  const struct symbol_mapping *hash_type = NULL;

  SCM_ASSERT (scm_is_symbol (type), type, SCM_ARG2, FUNC_NAME);

  scm_dynwind_begin (0);

  hash_type = _scm_to_ssh_const (hash_types, type);
  if (! hash_type)
    guile_ssh_error1 (FUNC_NAME, "Wrong type", type);

  res = ssh_get_publickey_hash (kd->ssh_key, hash_type->value,
                                &hash, &hash_len);
  scm_dynwind_free (hash);

  if (res == SSH_OK)
    {
      size_t idx;
      ret = scm_c_make_bytevector (hash_len);
      for (idx = 0; idx < hash_len; ++idx)
        scm_c_bytevector_set_x (ret, idx, hash[idx]);
    }
  else
    {
      ret = SCM_BOOL_F;
    }

  scm_dynwind_end ();
  return ret;
}
#undef FUNC_NAME


/* Initialize Scheme procedures. */
void
init_key_func (void)
{
#include "key-func.x"
}

/* key-func.c ends here */
