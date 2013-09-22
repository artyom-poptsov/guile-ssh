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
#include "key-type.h"

SCM_DEFINE (guile_ssh_message_reply_default,
            "message-reply-default", 1, 0, 0,
            (SCM msg),
            "")
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int res = ssh_message_reply_default (msg_data->message);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}


SCM_DEFINE (guile_ssh_message_service_reply_success,
            "message-service-reply-success", 1, 0, 0,
            (SCM msg),
            "")
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  /* TODO: implement this */
  //  int res = ssh_message_service_reply_success (
  return SCM_BOOL_F;
}


SCM_DEFINE (guile_ssh_message_auth_reply_success,
            "message-auth-reply-success", 2, 0, 0,
            (SCM msg, SCM partial_p),
            "")
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int c_partial_p = scm_to_bool (partial_p);
  int res = ssh_message_auth_reply_success (msg_data->message, c_partial_p);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM_DEFINE (guile_ssh_message_auth_reply_public_key_success,
            "message-auth-reply-public-key-success", 1, 0, 0,
            (SCM msg),
            "Answer OK to a public key auth request from message MSG.")
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int res = ssh_message_auth_reply_pk_ok_simple (msg_data->message);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}


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
            "")                 /* TODO: Add description */
{
  struct message_data *msg_data = _scm_to_ssh_message (msg);
  int res = ssh_message_channel_request_reply_success (msg_data->message);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}


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

SCM_DEFINE (guile_ssh_message_auth_get_user,
            "message-auth-get-user", 1, 0, 0,
            (SCM msg),
            "Get an user name from the message MSG.")
{
  struct message_data *message_data = _scm_to_ssh_message (msg);
  char *usr = ssh_message_auth_user (message_data->message);
  return usr ? scm_from_locale_string (usr) : SCM_BOOL_F;
}

SCM_DEFINE (guile_ssh_message_auth_get_password,
            "message-auth-get-password", 1, 0, 0,
            (SCM msg),
            "Get an user password from the message MSG.")
{
  struct message_data *message_data = _scm_to_ssh_message (msg);
  char *pswd = ssh_message_auth_password (message_data->message);
  return pswd ? scm_from_locale_symbol (pswd) : SCM_BOOL_F;
}


SCM_DEFINE (guile_ssh_message_auth_get_public_key,
            "message-auth-get-public-key", 1, 0, 0,
            (SCM msg),
            "Get the publickey of an auth request from message MSG")
{
  struct message_data *message_data = _scm_to_ssh_message (msg);
  SCM smob;
  struct key_data *key_data;

  key_data = (struct key_data *) scm_gc_malloc (sizeof (struct key_data),
                                                "ssh key");
  key_data->key_type = KEY_TYPE_PUBLIC;
  key_data->ssh_public_key = ssh_message_auth_publickey (message_data->message);
  if (key_data->ssh_public_key == NULL)
    return SCM_BOOL_F;

  SCM_NEWSMOB (smob, key_tag, key_data);

  return smob;
}


SCM_DEFINE (guile_ssh_message_auth_set_methods_x,
            "message-auth-set-methods!", 2, 0, 0,
            (SCM msg, SCM methods_list),
            "Set authentication methods.")
#define FUNC_NAME s_guile_ssh_message_auth_set_methods_x
{
  struct message_data *message_data = _scm_to_ssh_message (msg);
  int methods = 0;
  int res;

  SCM_ASSERT (scm_list_p (methods_list), methods_list, SCM_ARG2, FUNC_NAME);

  if (scm_is_true (scm_member (scm_from_locale_symbol ("password"),
                               methods_list)))
    {
      methods |= SSH_AUTH_METHOD_PASSWORD;
    }

  if (scm_is_true (scm_member (scm_from_locale_symbol ("public-key"),
                               methods_list)))
    {
      methods |= SSH_AUTH_METHOD_PUBLICKEY;
    }

  if (scm_is_true (scm_member (scm_from_locale_symbol ("interactive"),
                               methods_list)))
    {
      methods |= SSH_AUTH_METHOD_INTERACTIVE;
    }

  if (scm_is_true (scm_member (scm_from_locale_symbol ("host-based"),
                               methods_list)))
    {
      methods |= SSH_AUTH_METHOD_HOSTBASED;
    }

  res = ssh_message_auth_set_methods (message_data->message, methods);
  return (res >= 0) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


void
init_message_func (void)
{
#include "message-func.x"
}

/* message-func.c ends here */
