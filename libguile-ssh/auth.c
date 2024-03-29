/* auth.c -- User authentication procedures.
 *
 * Copyright (C) 2013-2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
#include <assert.h>

#include "error.h"
#include "session-type.h"
#include "key-type.h"
#include "key-func.h"
#include "log.h"


/* On the username:

   Some libssh functions (such as `ssh_userauth_password') expect
   username as one of the parameters.  But according to libssh 0.6
   docs, most server implementations do not permit changing the
   username during authentication.  Moreover, in some cases username
   parameter is already marked as deprecated in libssh 0.5.3.  So I
   decided to simplify the Guile-SSH Auth API and eliminate username
   from parameter list of functions of this module.  The username must
   be set by `session-set!' call.  - avp */


/**
 * Convert SSH authentication result to a Scheme symbol
 *
 * Return a symbol.
 *
 * Throws:
 * 'guile-ssh-error' on an error.
 *
 * Aseerts:
 * - 'res' is a valid libssh authentication result.
 */
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

    default:                    /* Must not happen. */
      _gssh_log_error_format(__func__,
                             SCM_BOOL_F,
                             "Unknown SSH result: %d",
                             res);
      assert (0);
      guile_ssh_error1(__func__,
                       "Unknown SSH result",
                       scm_from_int (res));
      return SCM_BOOL_F;
    }
}

SCM_DEFINE (guile_ssh_userauth_public_key_x, "userauth-public-key!", 2, 0, 0,
            (SCM session_smob,
             SCM private_key_smob),
            "Try to authenticate with a public key.\n"
            "Throw `wrong-type-arg' if a disconnected SESSION is passed\n"
            "as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_public_key_x
{
  gssh_session_t *session_data = gssh_session_from_scm (session_smob);
  gssh_key_t *private_key_data = gssh_key_from_scm (private_key_smob);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (session_data, session_smob, SCM_ARG1);
  SCM_ASSERT (_private_key_p (private_key_data),
              private_key_smob, SCM_ARG2, FUNC_NAME);

  res = ssh_userauth_publickey (session_data->ssh_session,
                                NULL, /* username */
                                private_key_data->ssh_key);

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_public_key_auto_x,
            "userauth-public-key/auto!", 1, 0, 0,
            (SCM session),
            "Try to automatically authenticate with \"none\" method first\n"
            "and then with public keys.  If the key is encrypted the user\n"
            "will be asked for a passphrase.  Return one of the following \n"
            "symbols: error, denied, partial, success.\n"
            "Throw `wrong-type-arg' if a disconnected SESSION is passed\n"
            "as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_public_key_auto_x
{
  gssh_session_t *sd = gssh_session_from_scm (session);
  GSSH_VALIDATE_CONNECTED_SESSION (sd, session, SCM_ARG1);

  int res = ssh_userauth_publickey_auto (sd->ssh_session,
                                         NULL,        /* username */
                                         NULL);       /* passphrase */
  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_public_key_try,
            "userauth-public-key/try", 2, 0, 0,
            (SCM session, SCM public_key),
            "Throw `wrong-type-arg' if a disconnected SESSION is passed\n"
            "as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_public_key_try
{
  gssh_session_t *sd = gssh_session_from_scm (session);
  gssh_key_t *kd = gssh_key_from_scm (public_key);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (sd, session, SCM_ARG1);
  SCM_ASSERT (_public_key_p (kd), public_key, SCM_ARG2, FUNC_NAME);

  if (! ssh_is_connected (sd->ssh_session))
    guile_ssh_error1 (FUNC_NAME, "Session is not connected", session);

  res = ssh_userauth_try_publickey (sd->ssh_session,
                                    NULL, /* username */
                                    kd->ssh_key);
  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_agent_x,
            "userauth-agent!", 1, 0, 0,
            (SCM session),
            "Throw `wrong-type-arg' if a disconnected SESSION is passed"
            " as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_agent_x
{
  gssh_session_t *sd = gssh_session_from_scm (session);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (sd, session, SCM_ARG1);

  res = ssh_userauth_agent (sd->ssh_session,
                            NULL /* username */ );

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

/* Try to authenticate by password. */
SCM_DEFINE (guile_ssh_userauth_password_x, "userauth-password!", 2, 0, 0,
            (SCM session, SCM password),
            "Try to authenticate by password.\n"
            "Throw `wrong-type-arg' if a disconnected SESSION is passed"
            " as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_password_x
{
  gssh_session_t* session_data = gssh_session_from_scm (session);
  char *c_password;
  int res;

  scm_dynwind_begin (0);

  /* Check types. */
  GSSH_VALIDATE_CONNECTED_SESSION (session_data, session, SCM_ARG1);
  SCM_ASSERT (scm_is_string (password), password, SCM_ARG2, FUNC_NAME);

  c_password = scm_to_locale_string (password);
  scm_dynwind_free (c_password);

  res = ssh_userauth_password (session_data->ssh_session,
                               NULL, /* username */
                               c_password);

  scm_dynwind_end ();

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_userauth_gssapi_x,
            "userauth-gssapi!", 1, 0, 0,
            (SCM session),
            "Try to authenticate through the \"gssapi-with-mic\" method."
            "Throw `wrong-type-arg' if a disconnected SESSION is passed"
            " as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_gssapi_x
{
  gssh_session_t *sd = gssh_session_from_scm (session);

  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (sd, session, SCM_ARG1);

  res = ssh_userauth_gssapi (sd->ssh_session);

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME


/* Try to authenticate through the "none" method.

   Return one of the following symbols: 'success, 'error, 'denied,
   'partial, 'again */
SCM_DEFINE (guile_ssh_userauth_none_x, "userauth-none!", 1, 0, 0,
            (SCM arg1),
            "Try to authenticate through the \"none\" method.\n"
            "Throw `wrong-type-arg' if a disconnected SESSION is passed"
            " as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_none_x
{
  gssh_session_t *session_data = gssh_session_from_scm (arg1);
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (session_data, arg1, SCM_ARG1);

  res = ssh_userauth_none (session_data->ssh_session,
                           NULL /* username */ );

  return ssh_auth_result_to_symbol (res);
}
#undef FUNC_NAME

/* Get available authentication methods for a session SESSION_SMOB.

   Return list of available methods. */
SCM_DEFINE (guile_ssh_userauth_get_list, "userauth-get-list", 1, 0, 0,
            (SCM session),
            "Get available authentication methods for a session SESSION.\n"
            "Throw `wrong-type-arg' if a disconnected SESSION is passed "
            " as an argument.")
#define FUNC_NAME s_guile_ssh_userauth_get_list
{
  gssh_session_t *session_data = gssh_session_from_scm (session);
  SCM auth_list = SCM_EOL;
  int res;

  GSSH_VALIDATE_CONNECTED_SESSION (session_data, session, SCM_ARG1);

  res = ssh_userauth_list (session_data->ssh_session,
                           NULL /* username */ );

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
#undef FUNC_NAME


/* Initialization */
void
init_auth_func (void)
{
#include "auth.x"
}

/* auth.c ends here */
