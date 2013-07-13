/* session-func.c -- Functions for working with SSH session.
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

/* SSH option mapping. */
struct option {
  char* symbol;
  int   type;
};

/* SSH options mapping to Guile symbols. */
static struct option session_options[] = {
  { "host",              SSH_OPTIONS_HOST               },
  { "port",              SSH_OPTIONS_PORT               },
  { "port-str",          SSH_OPTIONS_PORT_STR           },
  { "fd",                SSH_OPTIONS_FD                 },
  { "bindaddr",          SSH_OPTIONS_BINDADDR           },
  { "user",              SSH_OPTIONS_USER               },
  { "ssh-dir",           SSH_OPTIONS_SSH_DIR            },
  { "identity",          SSH_OPTIONS_IDENTITY           },
  { "add-identity",      SSH_OPTIONS_ADD_IDENTITY       },
  { "knownhosts",        SSH_OPTIONS_KNOWNHOSTS         },
  { "timeout",           SSH_OPTIONS_TIMEOUT            },
  { "timeout-usec",      SSH_OPTIONS_TIMEOUT_USEC       },
  { "ssh1",              SSH_OPTIONS_SSH1               },
  { "ssh2",              SSH_OPTIONS_SSH2               },
  { "log-verbosity",     SSH_OPTIONS_LOG_VERBOSITY      },
  { "log-verbosity-str", SSH_OPTIONS_LOG_VERBOSITY_STR  },
  { "ciphers-c-s",       SSH_OPTIONS_CIPHERS_C_S        },
  { "ciphers-s-c",       SSH_OPTIONS_CIPHERS_S_C        },
  { "compression-c-s",   SSH_OPTIONS_COMPRESSION_C_S    },
  { "compression-s-c",   SSH_OPTIONS_COMPRESSION_S_C    },
  { "proxycommand",      SSH_OPTIONS_PROXYCOMMAND       },
  { "strcthostkeycheck", SSH_OPTIONS_STRICTHOSTKEYCHECK },
  { "compression",       SSH_OPTIONS_COMPRESSION        },
  { "compression-level", SSH_OPTIONS_COMPRESSION_LEVEL  },
  { NULL,                -1 }
};

/* Blocking flush of the outgoing buffer.  

   Return on of the following symbols: 'ok, 'error. 'again. */
SCM
guile_ssh_blocking_flush (SCM session_smob, SCM timeout)
{
  struct session_data *data = _scm_to_ssh_session (session_smob);

  int c_timeout;                /* Timeout */
  int res;                      /* Result of a function call. */

  /* Check types */
  SCM_ASSERT (scm_is_integer (timeout), timeout, SCM_ARG2, __func__);

  c_timeout = scm_to_int (timeout);

  res = ssh_blocking_flush (data->ssh_session, c_timeout);
  switch (res)
    {
    case SSH_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_AGAIN:
      return scm_from_locale_symbol ("again");

    case SSH_ERROR:
    default:
      return scm_from_locale_symbol ("error");
    }
}


/* Set SSH session options */

/* Convert VALUE to a string and pass it to ssh_options_set */
static inline int
set_string_opt (ssh_session session, int type, SCM value)
{
  char *str;
  int ret;

  SCM_ASSERT (scm_is_string (value),  value, SCM_ARG3, __func__);

  str = scm_to_locale_string (value);
  ret = ssh_options_set (session, type, str);
  free (str);

  return ret;
}

/* Convert VALUE to uint64 and pass it to ssh_options_set */
static inline int
set_uint64_opt (ssh_session session, int type, SCM value)
{
  uint64_t c_value;

  SCM_ASSERT (scm_is_unsigned_integer (value, 0, UINT64_MAX), value,
              SCM_ARG3, __func__);

  c_value = scm_to_uint64 (value);
  return ssh_options_set (session, type, &c_value);
}

/* Convert VALUE to uint32 and pass it to ssh_options_set */
static inline int
set_uint32_opt (ssh_session session, int type, SCM value)
{
  unsigned int c_value;

  SCM_ASSERT (scm_is_unsigned_integer (value, 0, UINT32_MAX), value,
              SCM_ARG3, __func__);

  c_value = scm_to_uint32 (value);
  return ssh_options_set (session, type, &c_value);
}

/* Convert VALUE to int32 and pass it to ssh_options_set */
static inline int
set_int32_opt (ssh_session session, int type, SCM value)
{
  int32_t c_value;

  SCM_ASSERT (scm_is_integer (value), value, SCM_ARG3, __func__);

  c_value = scm_to_int (value);
  return ssh_options_set (session, type, &c_value);
}

/* Convert VALUE to integer that represents a boolan value (0
   considered as false, any other value is true), and pass it to
   ssh_options_set */
static inline int
set_bool_opt (ssh_session session, int type, SCM value)
{
  int32_t bool;

  SCM_ASSERT (scm_is_integer (value), value, SCM_ARG3, __func__);

  bool = scm_to_bool (value);
  return ssh_options_set (session, type, &bool);
}

/* Convert VALUE to a socket file descriptor and pass it to
   ssh_options_set */
static inline int
set_port_opt (ssh_session session, int type, SCM value)
{
  socket_t sfd;                 /* Socket File Descriptor */

  SCM_ASSERT (scm_port_p (value), value, SCM_ARG3, __func__);

  sfd = scm_to_int (scm_fileno (value));

  return ssh_options_set (session, type, &sfd);
}

/* Set an SSH session option. */
static int
set_option (ssh_session session, int type, SCM value)
{
  switch (type)
    {
    case SSH_OPTIONS_PORT:
      return set_uint32_opt (session, type, value);

    case SSH_OPTIONS_HOST:
    case SSH_OPTIONS_PORT_STR:
    case SSH_OPTIONS_BINDADDR:
    case SSH_OPTIONS_USER:
    case SSH_OPTIONS_COMPRESSION:
    case SSH_OPTIONS_LOG_VERBOSITY_STR:
    case SSH_OPTIONS_SSH_DIR:
    case SSH_OPTIONS_KNOWNHOSTS:
    case SSH_OPTIONS_IDENTITY:
    case SSH_OPTIONS_ADD_IDENTITY: /* Same as IDENTITY */
    case SSH_OPTIONS_CIPHERS_C_S:
    case SSH_OPTIONS_CIPHERS_S_C:
    case SSH_OPTIONS_COMPRESSION_C_S:
    case SSH_OPTIONS_COMPRESSION_S_C:
    case SSH_OPTIONS_PROXYCOMMAND:
      return set_string_opt (session, type, value);

    case SSH_OPTIONS_LOG_VERBOSITY:
    case SSH_OPTIONS_COMPRESSION_LEVEL:
      return set_int32_opt (session, type, value);

    case SSH_OPTIONS_TIMEOUT:
    case SSH_OPTIONS_TIMEOUT_USEC:
      return set_uint64_opt (session, type, value);

    case SSH_OPTIONS_SSH1:
    case SSH_OPTIONS_SSH2:
    case SSH_OPTIONS_STRICTHOSTKEYCHECK:
      return set_bool_opt (session, type, value);

    case SSH_OPTIONS_FD:
      return set_port_opt (session, type, value);

    default:
      guile_ssh_error1 (__func__, "Operation is not supported yet: %a~%",
                        scm_from_int (type));
    }

  return -1;                    /* ERROR */
}

/* Set a SSH option.  Return #t on success, #f on error. */
SCM
guile_ssh_session_set (SCM session, SCM type, SCM value)
{
  struct session_data* data = _scm_to_ssh_session (session);
  char *c_type_name;                    /* Name of an option */
  struct option *option;                /* SSH option mapping */
  int is_found = 0;                     /* Is a parameter found? */
  int res;                              /* Result of a function call */

  SCM_ASSERT (scm_is_symbol (type), type, SCM_ARG2, __func__);

  c_type_name = scm_to_locale_string (scm_symbol_to_string (type));

  for (option = session_options; option->symbol != NULL; ++option)
    {
      if (! strcmp (c_type_name, option->symbol))
        {
          is_found = 1;
          break;
        }
    }

  if(! is_found)
    return SCM_BOOL_F;

  res = set_option (data->ssh_session, option->type, value);

  scm_remember_upto_here_1 (session);

  return (res == 0) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Connect to the SSH server. 

   Return one of the following symbols: 'ok, 'error, 'again */
SCM
guile_ssh_connect (SCM arg1)
{
  struct session_data* data = _scm_to_ssh_session (arg1);
  int res = ssh_connect (data->ssh_session);
  switch (res)
    {
    case SSH_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_AGAIN:
      return scm_from_locale_symbol ("again");

    case SSH_ERROR:
    default:
      return scm_from_locale_symbol ("error");
    }
}

/* Disconnect from a session (client or server). 
   Return value is undefined.*/
SCM
guile_ssh_disconnect (SCM arg1)
{
  struct session_data* session_data = _scm_to_ssh_session (arg1);
  ssh_disconnect (session_data->ssh_session);
  return SCM_UNDEFINED;
}

/* Get SSH version.
 *
 * Return 1 for SSH1, 2 for SSH2 or #f on error
 */
SCM
guile_ssh_get_protocol_version (SCM arg1)
{
  struct session_data* data = _scm_to_ssh_session (arg1);
  SCM ret;
  int version = ssh_get_version (data->ssh_session);

  if (version >= 0)
    ret = scm_from_int (version);
  else
    ret = SCM_BOOL_F;

  return ret;
}

SCM
guile_ssh_get_error (SCM arg1)
{
  struct session_data* data = _scm_to_ssh_session (arg1);
  SCM error = scm_from_locale_string (ssh_get_error (data->ssh_session));
  return error;
}

/* Authenticate the server.  

   Return one of the following symbols: 'ok, 'known-changed,
   'found-other, 'not-known, 'file-not-fount, 'error */
SCM
guile_ssh_authenticate_server (SCM arg1)
{
  struct session_data* data = _scm_to_ssh_session (arg1);
  int res = ssh_is_server_known (data->ssh_session);

  switch (res)
    {
    case SSH_SERVER_KNOWN_OK:
      return scm_from_locale_symbol ("ok");

    case SSH_SERVER_KNOWN_CHANGED:
      return scm_from_locale_symbol ("known-changed");

    case SSH_SERVER_FOUND_OTHER:
      return scm_from_locale_symbol ("found-other");

    case SSH_SERVER_NOT_KNOWN:
      return scm_from_locale_symbol ("not-known");

    case SSH_SERVER_FILE_NOT_FOUND:
      return scm_from_locale_symbol ("file-not-found");

    case SSH_SERVER_ERROR:
    default:
      return scm_from_locale_symbol ("error");
    }
}

/* Get MD5 hash of the public key.

   Return MD5 hash on success, #f on error. */
SCM
guile_ssh_get_public_key_hash (SCM arg1)
{
  struct session_data *session_data = _scm_to_ssh_session (arg1);
  unsigned char *hash = NULL;
  char *hash_str;
  int res;
  SCM ret;

  scm_dynwind_begin (0);

  res = ssh_get_pubkey_hash (session_data->ssh_session, &hash);
  scm_dynwind_free (hash);

  hash_str = ssh_get_hexa (hash, res);
  scm_dynwind_free (hash_str);

  if (res >= 0)
    ret = scm_from_locale_string (hash_str);
  else
    ret = SCM_BOOL_F;

  scm_dynwind_end ();
  return ret;
}

/* Write the current server as known in the known hosts file. 

   Return #t on success, #f on error. */
SCM
guile_ssh_write_known_host (SCM arg1)
{
  struct session_data *session_data = _scm_to_ssh_session (arg1);
  int res = ssh_write_knownhost (session_data->ssh_session);
  return (res == SSH_OK) ? SCM_BOOL_T : SCM_BOOL_F;
}


/* Predicates */

/* Check if we are connected. 

   Return #f if we are connected to a server, #f if we aren't. */
SCM
guile_ssh_is_connected_p (SCM arg1)
{
  struct session_data* data = _scm_to_ssh_session (arg1);
  int res = ssh_is_connected (data->ssh_session);
  return scm_from_bool (res);
}


/* Initialize session related functions. */
void
init_session_func (void)
{
  scm_c_define_gsubr ("ssh:blocking-flush!", 2, 0, 0, guile_ssh_blocking_flush);
  scm_c_define_gsubr ("ssh:session-set!",    3, 0, 0, guile_ssh_session_set);
  scm_c_define_gsubr ("ssh:get-protocol-version", 1, 0, 0,
                      guile_ssh_get_protocol_version);
  scm_c_define_gsubr ("ssh:connect!",        1, 0, 0, guile_ssh_connect);
  scm_c_define_gsubr ("ssh:disconnect!",     1, 0, 0, guile_ssh_disconnect);
  scm_c_define_gsubr ("ssh:get-error",       1, 0, 0, guile_ssh_get_error);
  scm_c_define_gsubr ("ssh:authenticate-server", 1, 0, 0,
                      guile_ssh_authenticate_server);
  scm_c_define_gsubr ("ssh:get-public-key-hash", 1, 0, 0,
                      guile_ssh_get_public_key_hash);
  scm_c_define_gsubr ("ssh:write-known-host", 1, 0, 0,
                      guile_ssh_write_known_host);

  scm_c_define_gsubr ("ssh:connected?",      1, 0, 0, guile_ssh_is_connected_p);
}

/* session-func.c ends here */
