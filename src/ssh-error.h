#ifndef __SSH_ERROR_H__
#define __SSH_ERROR_H__

#define GUILE_SSH_EXCEPTION "guile-ssh-exception"

inline void ssh_error (const char *subr, char *message, SCM args, SCM rest);

#endif /* ifndef __SSH_ERROR_H__ */
