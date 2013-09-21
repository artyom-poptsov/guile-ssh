#ifndef __MESSAGE_TYPE_H__
#define __MESSAGE_TYPE_H__

#include <libguile.h>
#include <libssh/libssh.h>

extern scm_t_bits message_tag;


/* Smob data. */
struct message_data {
  ssh_message message;
};

extern void init_message_type (void);


/* Helper procedures. */
extern struct message_data *_scm_to_ssh_message (SCM x);

#endif  /* ifndef __MESSAGE_TYPE_H__ */

/* message-type.h ends here */
