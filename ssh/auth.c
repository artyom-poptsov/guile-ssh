/* auth.c -- User authentication procedures.
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

#include <libguile.h>
#include <libssh/libssh.h>

#include "error.h"
#include "session-type.h"
#include "key-type.h"
#include "key-func.h"


/* On the username:

   Some libssh functions (such as `ssh_userauth_password') expect
   username as one of the parameters.  But according to libssh 0.6
   docs, most server implementations do not permit changing the
   username during authentication.  Moreover, in some cases username
   parameter is already marked as deprecated in libssh 0.5.3.  So I
   decided to simplify the Guile-SSH Auth API and eliminate username
   from parameter list of functions of this module.  The username must
   be set by `session-set!' call.  - avp */


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

SCM_DEFINE (guile_ssh_userauth_public_key_x, "userauth-public-key!", 2, 0, 0,
            (SCM session_smob,
             SCM private_key_smob),
            "\
Try to authenticate with a public key.\
")
#define FUNC_NAME s_guile_ssh_userauth_public_key_x
{
  struct session_data *session_data = _scm_to_session_data (session_smob);
  struct key_data *private_key_data = _scm_to_key_data (private_key_smob);

  /* See "On the username" commentary above. */
  char *username = NULL;

  ssh_string public_key;
  int res;

  /* Check types. */
  SCM_ASSERT (_private_key_p (private_key_data),
              private_key_smob, SCM_ARG2, FUNC_NAME);

  res = ssh_userauth_publickey (session_data->ssh_session, username,
                                private_key_data->ssh_key);

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_public_key_auto_x,
            "userauth-public-key/auto!", 1, 0, 0,
            (SCM session),
            "\
Try to automatically authenticate with \"none\" method first and then with \n\
public keys.  If the key is encrypted the user will be asked for a \n\
passphrase.  Return one of the following symbols: error, denied, partial, \n\
success.\
")
#define FUNC_NAME s_guile_ssh_userauth_public_key_auto_x
{
  struct session_data *sd = _scm_to_session_data (session);
  char *username = NULL; /* See "On the username" commentary above. */
  char *passphrase = NULL;
  int res = ssh_userauth_publickey_auto (sd->ssh_session,
                                         username,
                                         passphrase); /* passphrase */
  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_public_key_try,
            "userauth-public-key/try", 2, 0, 0,
            (SCM session, SCM public_key),
            "")
#define FUNC_NAME s_guile_ssh_userauth_public_key_try
{
  struct session_data *sd = _scm_to_session_data (session);
  struct key_data *kd = _scm_to_key_data (public_key);
  char *username = NULL;        /* See "On the username" commentary above */
  int res;

  SCM_ASSERT (_public_key_p (kd), public_key, SCM_ARG2, FUNC_NAME);

  res = ssh_userauth_try_publickey (sd->ssh_session, username, kd->ssh_key);
  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_agent_x,
            "userauth-agent!", 1, 0, 0,
            (SCM session),
            /* FIXME: Fix the docsring. */
            "")
#define FUNC_NAME s_guile_ssh_userauth_agent_x
{
  struct session_data *sd = _scm_to_session_data (session);

  char *username = NULL; /* See "On the username" commentary above. */
  int res = ssh_userauth_agent (sd->ssh_session, username);

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

/* Try to authenticate by password. */
SCM_DEFINE (guile_ssh_userauth_password_x, "userauth-password!", 2, 0, 0,
            (SCM session, SCM password),
            "\
Try to authenticate by password.\
")
#define FUNC_NAME s_guile_ssh_userauth_password_x
{
  struct session_data* session_data = _scm_to_session_data (session);

  /* See "On the username" commentary above. */
  char *username = NULL;

  char *c_password;
  int res;

  scm_dynwind_begin (0);

  /* Check types. */
  SCM_ASSERT (scm_is_string (password), password, SCM_ARG2, FUNC_NAME);

  c_password = scm_to_locale_string (password);
  scm_dynwind_free (c_password);

  res = ssh_userauth_password (session_data->ssh_session,
                               username,
                               c_password);

  scm_dynwind_end ();

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME


/* Try to authenticate through the "none" method. 

   Return one of the following symbols: 'success, 'error, 'denied,
   'partial, 'again */
SCM_DEFINE (guile_ssh_userauth_none_x, "userauth-none!", 1, 0, 0,
            (SCM arg1),
            "\
Try to authenticate through the \"none\" method.\
")
{
  struct session_data *session_data = _scm_to_session_data (arg1);
  /* username is deprecated parameter.  Should be set to NULL. */
  int res = ssh_userauth_none (session_data->ssh_session, 
                               NULL); /* Username */
  return ssh_auth_result_to_symbol (res);
}

/* Get available authentication methods for a session SESSION_SMOB.

   Return list of available methods. */
SCM_DEFINE (guile_ssh_userauth_get_list, "userauth-get-list", 1, 0, 0,
            (SCM session),
            "\
Get available authentication methods for a session SESSION.\
")
{
  struct session_data *session_data = _scm_to_session_data (session);
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
