/* key-type.c -- SSH key smobs.
 *
 * Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

#include <config.h>

#include <libguile.h>
#include <libssh/libssh.h>

#include "key-type.h"
#include "common.h"
#include "error.h"

/* BUG: Currently a SSH key that has been read from a file has both
   public and private flags.  It means that we cannot distinguish
   whether the key is private or public by means of
   `ssh_key_is_private' and `ssh_key_is_public' procedures (they both
   return true).

   See `ssh_pki_import_privkey_file' and `pki_private_key_from_base64'
   in libssh 0.6.3 for details. -avp */

scm_t_bits key_tag; /* Smob tag. */

static const char* GSSH_KEY_TYPE_NAME = "key";

static const gssh_symbol_t key_types[] = {
  { "dss",     SSH_KEYTYPE_DSS     },
  { "rsa",     SSH_KEYTYPE_RSA     },
  { "rsa1",    SSH_KEYTYPE_RSA1    },
  { "ecdsa",   SSH_KEYTYPE_ECDSA   }, /* Deprecated in libssh 0.9 */

#if HAVE_LIBSSH_0_9
  { "ecdsa-p256",        SSH_KEYTYPE_ECDSA_P256        },
  { "ecdsa-p384",        SSH_KEYTYPE_ECDSA_P384        },
  { "ecdsa-p521",        SSH_KEYTYPE_ECDSA_P521        },
  { "ecdsa-p256-cert01", SSH_KEYTYPE_ECDSA_P256_CERT01 },
  { "ecdsa-p384-cert01", SSH_KEYTYPE_ECDSA_P384_CERT01 },
  { "ecdsa-p521-cert01", SSH_KEYTYPE_ECDSA_P521_CERT01 },
#endif

  { "ed25519", SSH_KEYTYPE_ED25519 },
  { "unknown", SSH_KEYTYPE_UNKNOWN },
  { NULL,      -1                  }
};

/* Smob marking */
static SCM
_mark (SCM key_smob)
{
  gssh_key_t *kd = gssh_key_from_scm (key_smob);
  return kd->parent;
}

/* Free the smob. */
static size_t
_free (SCM arg1)
{
  gssh_key_t *data = (gssh_key_t *) SCM_SMOB_DATA (arg1);

  if (scm_is_false (data->parent))
    {
      /* It's safe to free the key only if it was not derived from some other
         object and thereby does not share any resources with it.  If the key
         does have a parent then all the resources will be freed along with
         it. */
      ssh_key_free (data->ssh_key);
    }
  return 0;
}

static SCM
_equalp (SCM x1, SCM x2)
{
  return compare_objects(x1, x2, (converter_t) gssh_key_from_scm);
}

static int
_print (SCM smob, SCM port, scm_print_state *pstate)
{
  gssh_key_t *key_data = gssh_key_from_scm (smob);
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
  return gssh_symbol_to_scm (key_types, type);
}

const gssh_symbol_t *
_scm_to_ssh_key_type (SCM type)
{
  return gssh_symbol_from_scm (key_types, type);
}


/* Helper procedures */

gssh_key_t*
make_gssh_key ()
{
    return (gssh_key_t *) scm_gc_malloc (sizeof (gssh_key_t),
                                         GSSH_KEY_TYPE_NAME);
}

SCM
gssh_key_to_scm (ssh_key key, SCM parent)
{
  gssh_key_t *key_data;
  SCM key_smob;
  key_data = make_gssh_key ();
  key_data->ssh_key = key;
  key_data->parent = parent;
  SCM_NEWSMOB (key_smob, key_tag, key_data);
  return key_smob;
}

/* Convert X to a SSH key */
gssh_key_t *
gssh_key_from_scm (SCM x)
{
  scm_assert_smob_type (key_tag, x);
  return (gssh_key_t *) SCM_SMOB_DATA (x);
}

/* Check that KEY is a SSH private key. */
int
_private_key_p (gssh_key_t *key)
{
  return ssh_key_is_private (key->ssh_key);
}

/* Check that KEY is a SSH public key */
int
_public_key_p (gssh_key_t *key)
{
  return ssh_key_is_public (key->ssh_key);
}


/* Key smob initialization. */
void
init_key_type (void)
{
  key_tag = scm_make_smob_type (GSSH_KEY_TYPE_NAME, sizeof (gssh_key_t));
  set_smob_callbacks (key_tag, _mark, _free, _equalp, _print);

#include "key-type.x"
}

/* private-key.c ends here */
