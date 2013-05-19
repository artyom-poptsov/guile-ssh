/* auth.c -- SSH authentification procedures.
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

#include "session-type.h"
#include "key-type.h"

SCM
guile_ssh_userauth_pubkey (SCM session_smob, SCM username,
                           SCM public_key, SCM private_key_smob)
{
  struct session_data *session_data;
  struct key_data *private_key_data;
  char *c_username;
  char *c_public_key;
  int res;                      /* Result of a function call */
  SCM ret;

  scm_dynwind_begin (0);

  /* Check types. */
  scm_assert_smob_type (session_tag, session_smob);
  SCM_ASSERT (scm_is_string (public_key), public_key, SCM_ARG3, __func__);
  SCM_ASSERT (scm_to_bool (guile_ssh_is_private_key_p (private_key_smob)),
              private_key_smob, SCM_ARG4, __func__);

  session_data = (struct session_data *) SCM_SMOB_DATA (session_smob);
  private_key_data = (struct key_data *) SCM_SMOB_DATA (private_key_smob);

  c_public_key = scm_to_locale_string (public_key);
  scm_dynwind_free (c_public_key);

  if (scm_is_string (username))
    {
      c_username = scm_to_locale_string (username);
      scm_dynwind_free (c_username);
    }
  else if ((scm_is_bool (username)) && (! scm_to_bool (username)))
    {
      c_username = NULL;
    }
  else
    {
      guile_ssh_error1 (__func__, "Wrong argument: %a~%", username);
    }

  res = ssh_userauth_pubkey (session_data->ssh_session, c_username,
                             NULL,
                             private_key_data->ssh_private_key);

  switch (res)
    {
    case SSH_AUTH_ERROR:
      ret = scm_from_locale_symbol ("error");
      break;

    case SSH_AUTH_DENIED:
      ret = scm_from_locale_symbol ("denied");
      break;

    case SSH_AUTH_PARTIAL:
      ret = scm_from_locale_symbol ("partial");
      break;

    case SSH_AUTH_SUCCESS:
      ret = scm_from_locale_symbol ("success");
      break;
    }

  scm_dynwind_end ();
  return ret;
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
  SCM ret;

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

  switch (res)
    {
    case SSH_AUTH_SUCCESS:
      ret = scm_from_locale_symbol ("success");
      break;

    case SSH_AUTH_ERROR:
      ret = scm_from_locale_symbol ("error");
      break;

    case SSH_AUTH_DENIED:
      ret = scm_from_locale_symbol ("denied");
      break;

    case SSH_AUTH_PARTIAL:
      ret = scm_from_locale_symbol ("partial");
      break;

    case SSH_AUTH_AGAIN:
      ret = scm_from_locale_symbol ("again");
    }

  scm_dynwind_end ();

  return ret;
}


/* Initialization */
void
init_auth_func (void)
{
  scm_c_define_gsubr ("ssh:userauth-pubkey!", 4, 0, 0,
                      guile_ssh_userauth_pubkey);
  scm_c_define_gsubr ("ssh:userauth-password!", 3, 0, 0,
                      guile_ssh_userauth_password);
}

/* auth.c ends here */
