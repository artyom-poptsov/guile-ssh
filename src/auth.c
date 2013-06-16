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
SCM
guile_ssh_userauth_pubkey (SCM session_smob, SCM username,
                           SCM public_key_smob, SCM private_key_smob)
{
  struct session_data *session_data;
  struct key_data *private_key_data;
  struct key_data *public_key_data;
  char *c_username = NULL;
  ssh_string public_key;
  int res;

  scm_dynwind_begin (0);

  /* Check types. */
  scm_assert_smob_type (session_tag, session_smob);
  /* username can be either a string or SCM_BOOL_F */
  SCM_ASSERT (scm_is_string (username)
              || (scm_is_bool (username) && (! scm_to_bool (username))),
              username, SCM_ARG2, __func__);
  SCM_ASSERT (scm_to_bool (guile_ssh_is_public_key_p (public_key_smob)),
              public_key_smob, SCM_ARG3, __func__);
  SCM_ASSERT (scm_to_bool (guile_ssh_is_private_key_p (private_key_smob)),
              private_key_smob, SCM_ARG4, __func__);

  session_data     = (struct session_data *) SCM_SMOB_DATA (session_smob);
  private_key_data = (struct key_data *) SCM_SMOB_DATA (private_key_smob);
  public_key_data  = (struct key_data *) SCM_SMOB_DATA (public_key_smob);

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

/* Try to authenticate by password.
 *
 * USERNAME can be either string or #f.  If USERNAME is #f it's assumed that
 * the USERNAME was set through ssh:option-set! call.
 *
 */
SCM
guile_ssh_userauth_password (SCM session_smob, SCM username, SCM password)
{
  struct session_data* session_data;
  char *c_username;
  char *c_password;
  int res;

  scm_dynwind_begin (0);

  /* Check types. */
  scm_assert_smob_type (session_tag, session_smob);
  SCM_ASSERT ((scm_is_string (username) || scm_is_bool (username)),
              password, SCM_ARG2, __func__);
  SCM_ASSERT (scm_is_string (password), password, SCM_ARG3, __func__);

  session_data = (struct session_data *) SCM_SMOB_DATA (session_smob);

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


/* Try to authenticate through the "none" method. 

   Return one of the following symbols: 'success, 'error, 'denied,
   'partial, 'again */
SCM
guile_ssh_userauth_none (SCM session_smob)
{
  struct session_data *session_data;
  int res;

  scm_assert_smob_type (session_tag, session_smob);

  session_data = (struct session_data *) SCM_SMOB_DATA (session_smob);

  /* username is deprecated parameter.  Should be set to NULL. */
  res = ssh_userauth_none (session_data->ssh_session, 
                           NULL); /* Username */

  return ssh_auth_result_to_symbol (res);
}

/* Get available authentication methods for a session SESSION_SMOB.

   Return list of available methods. */
SCM
guile_ssh_userauth_get_list (SCM session_smob)
{
  struct session_data *session_data;
  SCM auth_list = SCM_EOL;
  int res;

  scm_assert_smob_type (session_tag, session_smob);

  session_data = (struct session_data *) SCM_SMOB_DATA (session_smob);

  /* The second argument of the function is a username.  According to
     the documentation for libssh 0.5.3, this argument is deprecated
     and must be set to NULL. */
  res = ssh_userauth_list (session_data->ssh_session, NULL);

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
  scm_c_define_gsubr ("ssh:userauth-pubkey!", 4, 0, 0,
                      guile_ssh_userauth_pubkey);
  scm_c_define_gsubr ("ssh:userauth-password!", 3, 0, 0,
                      guile_ssh_userauth_password);
  scm_c_define_gsubr ("ssh:userauth-none!", 1, 0, 0,
                      guile_ssh_userauth_none);

  scm_c_define_gsubr ("ssh:userauth-get-list", 1, 0, 0,
                      guile_ssh_userauth_get_list);
}

/* auth.c ends here */
