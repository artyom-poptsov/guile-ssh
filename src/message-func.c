/* message-func.c -- Functions for working with SSH messages.
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
#include <libssh/server.h>

#include "channel-type.h"
#include "message-type.h"
#include "message-func.h"
#include "key-type.h"
#include "error.h"

SCM_DEFINE (guile_ssh_message_reply_default,
            "message-reply-default", 1, 0, 0,
            (SCM msg),
            "Reduced version of the reply default that only reply with "
            "SSH_MSG_UNIMPLEMENTED.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_message_reply_default
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int res = ssh_message_reply_default (msg_data->message);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Unable to reply", msg);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_message_service_reply_success,
            "message-service-reply-success", 1, 0, 0,
            (SCM msg),
            "")
#define FUNC_NAME s_guile_ssh_message_service_reply_success
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int res = ssh_message_service_reply_success (msg_data->message);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Unable to reply", msg);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_message_auth_reply_success,
            "message-auth-reply-success", 2, 0, 0,
            (SCM msg, SCM partial_p),
            "TODO: Add description.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_message_auth_reply_success
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int c_partial_p = scm_to_bool (partial_p);
  int res = ssh_message_auth_reply_success (msg_data->message, c_partial_p);
  if (res != SSH_OK)
    {
      guile_ssh_error1 (FUNC_NAME, "Unable to reply",
                        scm_list_2 (msg, partial_p));
    }
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (guile_ssh_message_auth_reply_public_key_success,
            "message-auth-reply-public-key-success", 1, 0, 0,
            (SCM msg),
            "Answer OK to a public key auth request from message MSG.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_message_auth_reply_public_key_success
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int res = ssh_message_auth_reply_pk_ok_simple (msg_data->message);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Unable to reply", msg);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (guile_ssh_message_channel_request_open_reply_accept,
            "message-channel-request-open-reply-accept", 1, 0, 0,
            (SCM msg),
            "")
{
  SCM smob;
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  struct channel_data *channel_data
    = (struct channel_data *) scm_gc_malloc (sizeof (struct channel_data),
                                             "channel");
  channel_data->ssh_channel
    = ssh_message_channel_request_open_reply_accept (msg_data->message);
  if (channel_data->ssh_channel == NULL)
    return SCM_BOOL_F;

  SCM_NEWSMOB (smob, channel_tag, channel_data);

  return smob;
}


SCM_DEFINE (guile_ssh_message_channel_request_reply_success,
            "message-channel-request-reply-success", 1, 0, 0,
            (SCM msg),
            "TODO: Add description.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_message_channel_request_reply_success
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int res = ssh_message_channel_request_reply_success (msg_data->message);
  if (res != SSH_OK)
    guile_ssh_error1 (FUNC_NAME, "Unable to reply", msg);
  return SCM_UNDEFINED;
}
#undef FUNC_NAME


struct symbol_mapping {
  char* symbol;
  int   value;
};

static struct symbol_mapping req_types[] = {
  { "request-auth",         SSH_REQUEST_AUTH         },
  { "request-channel-open", SSH_REQUEST_CHANNEL_OPEN },
  { "request-channel",      SSH_REQUEST_CHANNEL      },
  { "request-service",      SSH_REQUEST_SERVICE      },
  { "request-global",       SSH_REQUEST_GLOBAL       },
  { NULL,           -1                               }
};

static struct symbol_mapping req_auth_subtypes[] = {
  { "auth-method-unknown",     SSH_AUTH_METHOD_UNKNOWN     },
  { "auth-method-none",        SSH_AUTH_METHOD_NONE        },
  { "auth-method-password",    SSH_AUTH_METHOD_PASSWORD    },
  { "auth-method-publickey",   SSH_AUTH_METHOD_PUBLICKEY   },
  { "auth-method-hostbased",   SSH_AUTH_METHOD_HOSTBASED   },
  { "auth-method-interactive", SSH_AUTH_METHOD_INTERACTIVE },
  { NULL,                      -1                          }
};

static struct symbol_mapping req_channel_subtypes[] = {
  { "channel-request-unknown",       SSH_CHANNEL_REQUEST_UNKNOWN       },
  { "channel-request-pty",           SSH_CHANNEL_REQUEST_PTY           },
  { "channel-request-exec",          SSH_CHANNEL_REQUEST_EXEC          },
  { "channel-request-shell",         SSH_CHANNEL_REQUEST_SHELL         },
  { "channel-request-env",           SSH_CHANNEL_REQUEST_ENV           },
  { "channel-request-subsystem",     SSH_CHANNEL_REQUEST_SUBSYSTEM     },
  { "channel-request-window-change", SSH_CHANNEL_REQUEST_WINDOW_CHANGE },
  { NULL,                            -1                                }
};

static struct symbol_mapping req_channel_open_subtypes[] = {
  { "channel-unknown",         SSH_CHANNEL_UNKNOWN         },
  { "channel-session",         SSH_CHANNEL_SESSION         },
  { "channel-direct-tcpip",    SSH_CHANNEL_DIRECT_TCPIP    },
  { "channel-forwarded-tcpip", SSH_CHANNEL_FORWARDED_TCPIP },
  { "channel-x11",             SSH_CHANNEL_X11             },
  { NULL,                      -1                          }
};

static struct symbol_mapping req_global_subtypes[] = {
  { "global-request-unknown",              SSH_GLOBAL_REQUEST_UNKNOWN              },
  { "global-request-tcpip-forward",        SSH_GLOBAL_REQUEST_TCPIP_FORWARD        },
  { "global-request-cancel-tcpip-forward", SSH_GLOBAL_REQUEST_CANCEL_TCPIP_FORWARD },
  { NULL,                                  -1                                      }
};

/* Convert the SSH constant VALUE to a Scheme symbol */
static SCM
_ssh_const_to_scm (const struct symbol_mapping *types, int value)
{
  struct symbol_mapping *t;
  for (t = types; t->symbol; ++t)
    {
      if (t->value == value)
        return scm_from_locale_symbol (t->symbol);
    }
  return SCM_BOOL_F;
}

/* Get a type of the message MSG as a list.  car of the list is type
   of the message, cdr is a subtype.

   Return #f on error. */
static SCM
_ssh_message_type_to_scm (ssh_message msg)
{
  int type     = ssh_message_type (msg);
  int subtype  = ssh_message_subtype (msg);
  SCM scm_type = _ssh_const_to_scm (req_types, type);
  SCM scm_subtype;

  switch (type)
    {
    case SSH_REQUEST_AUTH:
      scm_subtype = _ssh_const_to_scm (req_auth_subtypes, subtype);
      return scm_list_2 (scm_type, scm_subtype);

    case SSH_REQUEST_CHANNEL_OPEN:
      scm_subtype = _ssh_const_to_scm (req_channel_open_subtypes, subtype);
      return scm_list_2 (scm_type, scm_subtype);

    case SSH_REQUEST_CHANNEL:
      scm_subtype = _ssh_const_to_scm (req_channel_subtypes, subtype);
      return scm_list_2 (scm_type, scm_subtype);

    case SSH_REQUEST_GLOBAL:
      scm_subtype = _ssh_const_to_scm (req_global_subtypes, subtype);
      return scm_list_2 (scm_type, scm_subtype);

    case SSH_REQUEST_SERVICE:
      return scm_list_1 (scm_type);

    default:
      return SCM_BOOL_F;
    }
}


SCM_DEFINE (guile_ssh_message_get_type,
            "message-get-type", 1, 0, 0,
            (SCM msg),
            "Get type of the message MSG.")
{
  struct message_data *message_data = _scm_to_ssh_message (msg);
  return _ssh_message_type_to_scm (message_data->message);
}


/* These procedures return an object that represents a SSH request. */

/* Get the content of authentication request as a vector of the
   following format:
     <result> = "#(" <user> <WSP> <password> <WSP> <key> ")" */
static SCM
get_auth_req (ssh_message msg)
{
  SCM result = scm_c_make_vector (3, SCM_UNDEFINED);
  char *user     = ssh_message_auth_user (msg);
  char *password = ssh_message_auth_password (msg);
  ssh_public_key public_key = ssh_message_auth_publickey (msg);
  SCM pkey_smob;
  struct key_data *pkey_data;

  if (user)
    SCM_SIMPLE_VECTOR_SET (result, 0, scm_from_locale_string (user));
  else
    SCM_SIMPLE_VECTOR_SET (result, 0, SCM_BOOL_F);

  if (password)
    SCM_SIMPLE_VECTOR_SET (result, 1, scm_from_locale_string (password));
  else
    SCM_SIMPLE_VECTOR_SET (result, 1, SCM_BOOL_F);

  pkey_data = (struct key_data *) scm_gc_malloc (sizeof (struct key_data),
                                                 "ssh key");
  pkey_data->key_type = KEY_TYPE_PUBLIC;
  pkey_data->ssh_public_key = public_key;

  SCM_NEWSMOB (pkey_smob, key_tag, pkey_data);

  SCM_SIMPLE_VECTOR_SET (result, 2, pkey_smob);

  return result;
}

/* Get the content of a pty request as a vector of the following
   format:
     <result> = "#(" <term> <WSP> <width> <WSP> <height> <WSP>
                <pxwidth> <WSP> <pxheight> ")" */
static SCM
get_pty_req (ssh_message msg)
{
  SCM result = scm_c_make_vector (5, SCM_UNDEFINED);
  char *term = ssh_message_channel_request_pty_term (msg);
  int w   = ssh_message_channel_request_pty_width (msg);
  int h   = ssh_message_channel_request_pty_height (msg);
  int pxw = ssh_message_channel_request_pty_pxwidth (msg);
  int pxh = ssh_message_channel_request_pty_pxheight (msg);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (term));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_from_int (w));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_int (h));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_int (pxw));
  SCM_SIMPLE_VECTOR_SET(result, 4, scm_from_int (pxh));

  return result;
}

/* Get the content of an env request as a vector of the following
   format:
     <result> = "#(" <name> <WSP> <value> ")" */
static SCM
get_env_req (ssh_message msg)
{
  SCM result  = scm_c_make_vector (3, SCM_UNDEFINED);
  char *name  = ssh_message_channel_request_env_name (msg);
  char *value = ssh_message_channel_request_env_value (msg);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_from_locale_string (value));

  return result;
}

/* Get the content of an exec request as a vector of the following
   format:
     <result> = "#(" <cmd> ")" */
static SCM
get_exec_req (ssh_message msg)
{
  SCM result = scm_c_make_vector (1, SCM_UNDEFINED);
  char *cmd = ssh_message_channel_request_command (msg);
  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (cmd));
  return result;
}

/* Get the content of a global request as a vector of the following
   format:
     <result> = "#(" <addr> <WSP> <port> ")" */
static SCM
get_global_req (ssh_message msg)
{
  SCM result = scm_c_make_vector (2, SCM_UNDEFINED);
  char *addr = ssh_message_global_request_address (msg);
  int port = ssh_message_global_request_port (msg);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (addr));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_from_int (port));

  return result;
}

SCM_DEFINE (guile_ssh_message_get_req,
            "message-get-req", 1, 0, 0,
            (SCM msg),
            "Get a request object from the message MSG")
#define FUNC_NAME s_guile_ssh_message_get_req
{
  struct message_data *message_data = _scm_to_ssh_message (msg);
  ssh_message ssh_msg = message_data->message;
  int type = ssh_message_type (ssh_msg);

  switch (type)
    {
    case SSH_REQUEST_AUTH:
      return get_auth_req (ssh_msg);

    case SSH_REQUEST_CHANNEL:
      {
        int subtype = ssh_message_subtype (ssh_msg);
        switch (subtype)
          {
          case SSH_CHANNEL_REQUEST_PTY:
            return get_pty_req (ssh_msg);

          case SSH_CHANNEL_REQUEST_EXEC:
            return get_exec_req (ssh_msg);

          case SSH_CHANNEL_REQUEST_ENV:
            return get_env_req (ssh_msg);

          default:
            guile_ssh_error1 (FUNC_NAME, "Wrong message subtype",
                              scm_from_int (subtype));
          }
      }

    case SSH_REQUEST_GLOBAL:
      return get_global_req (ssh_msg);

    default:
      guile_ssh_error1 (FUNC_NAME, "Wrong message type",
                        _ssh_const_to_scm (req_types, type));

    }

  return SCM_BOOL_F;            /* Never reached. */
}
#undef FUNC_NAME


/* A convenient wrapper for `scm_member' that returns its result as
   int. */
static inline int
_scm_member_p (SCM elem, SCM lst)
{
  return scm_is_true (scm_member (elem, lst));
}

SCM_DEFINE (guile_ssh_message_auth_set_methods_x,
            "message-auth-set-methods!", 2, 0, 0,
            (SCM msg, SCM methods_list),
            "Set authentication methods.\n"
            "Return value is undefined.")
#define FUNC_NAME s_guile_ssh_message_auth_set_methods_x
{
  struct message_data *message_data = _scm_to_ssh_message (msg);
  int methods = 0;
  int res;

  SCM_ASSERT (scm_list_p (methods_list), methods_list, SCM_ARG2, FUNC_NAME);

  if (_scm_member_p (scm_from_locale_symbol ("password"), methods_list))
    methods |= SSH_AUTH_METHOD_PASSWORD;

  if (_scm_member_p (scm_from_locale_symbol ("public-key"), methods_list))
    methods |= SSH_AUTH_METHOD_PUBLICKEY;

  if (_scm_member_p (scm_from_locale_symbol ("interactive"), methods_list))
    methods |= SSH_AUTH_METHOD_INTERACTIVE;

  if (_scm_member_p (scm_from_locale_symbol ("host-based"), methods_list))
    methods |= SSH_AUTH_METHOD_HOSTBASED;

  res = ssh_message_auth_set_methods (message_data->message, methods);
  if (res != SSH_OK)
    {
      guile_ssh_error1 (FUNC_NAME, "Unable to set auth methods",
                        scm_list_2 (msg, methods_list));
    }

  return SCM_UNDEFINED;
}
#undef FUNC_NAME


void
init_message_func (void)
{
#include "message-func.x"
}

/* message-func.c ends here */
