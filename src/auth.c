/* auth.c -- User authentication procedures.
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

#include "error.h"
#include "session-type.h"
#include "key-type.h"
#include "key-func.h"


/* Convert SSH authentication result to a Scheme symbol 

   Return a symbol, or #f on error. */
static SCM
ssh_auth_result_to_symbol (const int res)
{
  switch (res)
    {
    case SSH_AUTH_SUCCESS:
      return scm_from_locale_symbol ("success");

    case SSH_AUTH_ERROR:
      return scm_from_locale_symbol ("error");

    case SSH_AUTH_DENIED:
      return scm_from_locale_symbol ("denied");

    case SSH_AUTH_PARTIAL:
      return scm_from_locale_symbol ("partial");

    case SSH_AUTH_AGAIN:
      return scm_from_locale_symbol ("again");

    default:
      return SCM_BOOL_F;
    }
}

/* Try to authenticate with a public key.
 *
 * USERNAME can be either string or #f.  If USERNAME is #f it's assumed that
 * the USERNAME was set through ssh:option-set! call.
 */
SCM_DEFINE (guile_ssh_userauth_pubkey, "userauth-pubkey!", 4, 0, 0,
            (SCM session_smob, SCM username,
             SCM public_key_smob, SCM private_key_smob),
            "Try to authenticate with a public key.\n"
            "\n"
            "USERNAME can be either string or #f,  If USERNAME is #f it's\n"
            "assumed that the USERNAME was set though `option-set!' call.")
#define FUNC_NAME s_guile_ssh_userauth_pubkey
{
  struct session_data *session_data = _scm_to_ssh_session (session_smob);
  struct key_data *public_key_data  = _scm_to_ssh_key (public_key_smob);
  struct key_data *private_key_data = _scm_to_ssh_key (private_key_smob);
  char *c_username = NULL;
  ssh_string public_key;
  int res;

  scm_dynwind_begin (0);

  /* Check types. */

  /* username can be either a string or SCM_BOOL_F */
  SCM_ASSERT (scm_is_string (username)
              || (scm_is_bool (username) && (! scm_to_bool (username))),
              username, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (_public_key_p (public_key_data),
              public_key_smob, SCM_ARG3, FUNC_NAME);
  SCM_ASSERT (_private_key_p (private_key_data),
              private_key_smob, SCM_ARG4, FUNC_NAME);

  if (scm_is_string (username))
    {
      c_username = scm_to_locale_string (username);
      scm_dynwind_free (c_username);
    }
  else                          /* username was set to SCM_BOOL_F */
    {
      c_username = NULL;
    }

  public_key = public_key_to_ssh_string (public_key_data);
  scm_dynwind_unwind_handler ((void (*)(void*)) ssh_string_free, public_key,
                                  SCM_F_WIND_EXPLICITLY);

  res = ssh_userauth_pubkey (session_data->ssh_session, c_username,
                             public_key,
                             private_key_data->ssh_private_key);

  scm_dynwind_end ();

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_autopubkey_x,
            "userauth-autopubkey!", 1, 1, 0,
            (SCM session, SCM passphrase),
            "Try to automatically authenticate with a public key or \"none\" "
            "method.  The user can provide a PASSPHRASE if she wants to use "
            "an encrypted private key for authentication.  If passphrase is "
            "not provided it's assumed that either the key does not protected "
            "or the user shoud be asked for the passphrase interactively.\n"
            "\n"
            "Return one of the following symbols: error, denied, partial, "
            "success.")
#define FUNC_NAME s_guile_ssh_userauth_autopubkey_x
{
  struct session_data *sd = _scm_to_ssh_session (session);
  /* NULL means that either the public key is unprotected or the user
     should be asked for the passphrase. */
  char *c_passphrase = NULL;

  if (! SCM_UNBNDP (passphrase))
    {
      SCM_ASSERT (scm_is_string (passphrase), passphrase, SCM_ARG2, FUNC_NAME);
      c_passphrase = scm_to_locale_string (passphrase);
    }

  int res = ssh_userauth_autopubkey (sd->ssh_session, c_passphrase);
  free (c_passphrase);

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

/* Try to authenticate by password.
 *
 * USERNAME can be either string or #f.  If USERNAME is #f it's assumed that
 * the USERNAME was set through ssh:option-set! call.
 *
 */
SCM_DEFINE (guile_ssh_userauth_password, "userauth-password!", 3, 0, 0,
            (SCM session, SCM username, SCM password),
            "Try to authenticate by password.")
#define FUNC_NAME s_guile_ssh_userauth_password
{
  struct session_data* session_data = _scm_to_ssh_session (session);
  char *c_username;
  char *c_password;
  int res;

  scm_dynwind_begin (0);

  /* Check types. */
  SCM_ASSERT ((scm_is_string (username) || scm_is_bool (username)),
              password, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (scm_is_string (password), password, SCM_ARG3, FUNC_NAME);

  if (scm_is_true (username))
    {
      c_username = scm_to_locale_string (username);
      scm_dynwind_free (c_username);
    }
  else
    {
      /* Username was set by calling ssh_options_set */
      c_username = NULL;
    }

  c_password = scm_to_locale_string (password);
  scm_dynwind_free (c_password);

  res = ssh_userauth_password (session_data->ssh_session,
                               c_username,
                               c_password);

  scm_dynwind_end ();

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME


/* Try to authenticate through the "none" method. 

   Return one of the following symbols: 'success, 'error, 'denied,
   'partial, 'again */
SCM_DEFINE (guile_ssh_userauth_none, "userauth-none!", 1, 0, 0,
            (SCM arg1),
            "Try to authenticate through the \"none\" method.")
{
  struct session_data *session_data = _scm_to_ssh_session (arg1);
  /* username is deprecated parameter.  Should be set to NULL. */
  int res = ssh_userauth_none (session_data->ssh_session, 
                               NULL); /* Username */
  return ssh_auth_result_to_symbol (res);
}

/* Get available authentication methods for a session SESSION_SMOB.

   Return list of available methods. */
SCM_DEFINE (guile_ssh_userauth_get_list, "userauth-get-list", 1, 0, 0,
            (SCM session),
            "Get available authentication methods for a session SESSION.")
{
  struct session_data *session_data = _scm_to_ssh_session (session);
  SCM auth_list = SCM_EOL;

  /* The second argument of the function is a username.  According to
     the documentation for libssh 0.5.3, this argument is deprecated
     and must be set to NULL. */
  int res = ssh_userauth_list (session_data->ssh_session, NULL);

  if (res & SSH_AUTH_METHOD_PASSWORD)
    {
      SCM method = scm_from_locale_symbol ("password");
      auth_list = scm_append (scm_list_2 (auth_list, scm_list_1 (method)));
    }

  if (res & SSH_AUTH_METHOD_PUBLICKEY)
    {
      SCM method = scm_from_locale_symbol ("public-key");
      auth_list = scm_append (scm_list_2 (auth_list, scm_list_1 (method)));
    }

  if (res & SSH_AUTH_METHOD_HOSTBASED)
    {
      SCM method = scm_from_locale_symbol ("host-based");
      auth_list = scm_append (scm_list_2 (auth_list, scm_list_1 (method)));
    }

  if (res & SSH_AUTH_METHOD_INTERACTIVE)
    {
      SCM method = scm_from_locale_symbol ("interactive");
      auth_list = scm_append (scm_list_2 (auth_list, scm_list_1 (method)));
    }

  return auth_list;
}


/* Initialization */
void
init_auth_func (void)
{
#include "auth.x"
}

/* auth.c ends here */
