/* fac.c */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 100

int main(int argc, char **argv) {
  int fd;                       /* file descriptor of Erlang node */

  int loop = 1;                 /* Loop flag                      */
  int got;                      /* Result of receive              */
  unsigned char buf[BUFSIZE];   /* Buffer for incoming message    */
  ErlMessage emsg;              /* Incoming message               */

  ETERM *fromp, *argp, *resp;   /* Reps of Erlang terms           */
  int res;                      /* Result of the fac call         */

    /* initialize erl_interface (once only) */           
  erl_init(NULL, 0);  
         
    /* initialize the connection mechanism  */
  if (erl_connect_init(1, "refactorcookie", 0) == -1)
    erl_err_quit("erl_connect_init");

    /* connect to a running Erlang node     */
  if ((fd = erl_connect("blah@Simon-Thompsons-Computer-2")) < 0)
    erl_err_quit("erl_connect");

  while (loop) {
      /* message received */
    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);

    if (got == ERL_TICK) {
      /* ignore */
    } else if (got == ERL_ERROR) {
      loop = 0;
    } else {
      if (emsg.type == ERL_REG_SEND) {
	  /* unpack message fields         */
        fromp = erl_element(1, emsg.msg);
        argp = erl_element(2, emsg.msg);

	  /* call fac and send result back */
        resp = erl_format("{ok, ~i}", fac(ERL_INT_VALUE(argp)));
        erl_send(fd, fromp, resp);

	  /* free the term storage used    */
        erl_free_term(emsg.from); erl_free_term(emsg.msg);
        erl_free_term(fromp); erl_free_term(argp);
        erl_free_term(resp);
      }
    }
  }
}


int fac(int y) {
  if (y <= 0) 
    {return 1;}
  else
    {return (y*fac(y-1));};
}
