/* Created 07/March/2004 Chandrashekhar Mullaparthi

   $Id: ibrowse_drv.c,v 1.1 2005/05/05 22:28:28 chandrusf Exp $
   
   Erlang Linked in driver to URL encode a set of data
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "erl_driver.h"

static ErlDrvData ibrowse_drv_start(ErlDrvPort port, char* buff);
static void ibrowse_drv_stop(ErlDrvData handle);
static void ibrowse_drv_command(ErlDrvData handle, char *buff, int bufflen);
static void ibrowse_drv_finish(void);
static int ibrowse_drv_control(ErlDrvData handle, unsigned int command, 
                       char* buf, int count, char** res, int res_size);

/* The driver entry */
static ErlDrvEntry ibrowse_driver_entry = {
    NULL,                          /* init, N/A */
    ibrowse_drv_start,             /* start, called when port is opened */
    ibrowse_drv_stop,              /* stop, called when port is closed */
    NULL,                          /* output, called when erlang has sent */
    NULL,                          /* ready_input, called when input descriptor 
				      ready */
    NULL,                          /* ready_output, called when output 
				      descriptor ready */
    "ibrowse_drv",                 /* char *driver_name, the argument 
				      to open_port */
    NULL,                          /* finish, called when unloaded */
    NULL,                          /* void * that is not used (BC) */
    ibrowse_drv_control,           /* control, port_control callback */
    NULL,                          /* timeout, called on timeouts */
    NULL,                          /* outputv, vector output interface */
    NULL,
    NULL,
    NULL,                          /* call, synchronous call to driver */
    NULL
};

typedef struct ibrowse_drv_data {
  unsigned int count;
  void *alloc_ptr;
} State;

static State *ibrowse_drv_data;

DRIVER_INIT(ibrowse_drv)
{
  ibrowse_drv_data = NULL;
  return &ibrowse_driver_entry;
}

static ErlDrvData ibrowse_drv_start(ErlDrvPort port, char *buff)
{
  State *state;

  state = driver_alloc(sizeof(State));
  state->count = 0;
  state->alloc_ptr = NULL;

  ibrowse_drv_data = state;
  return ((ErlDrvData) state);
}

void ibrowse_drv_stop(ErlDrvData desc)
{
  return;
}

static int ibrowse_drv_control(ErlDrvData handle, unsigned int command,
			       char *buf, int bufflen, char **rbuf, int rlen)
{
  State* state = (State *) handle;
  unsigned int j = 0, i = 0;
  unsigned int temp = 0, rlen_1 = 0;
  char* replybuf;

  fprintf(stderr, "alloc_ptr -> %d\n", state->alloc_ptr);
/*   if(state->alloc_ptr != NULL) */
/*     { */
/*       driver_free(state->alloc_ptr); */
/*     } */

  /* Calculate encoded length. If same as bufflen, it means there is
     no encoding to do. Do return an empty list */
  rlen_1 = calc_encoded_length(buf, bufflen);
  if(rlen_1 == bufflen)
    {
      *rbuf = NULL;
      state->alloc_ptr = NULL;
      return 0;
    }
  *rbuf = driver_alloc(rlen_1);
  state->alloc_ptr = *rbuf;
  fprintf(stderr, "*rbuf -> %d\n", *rbuf);
  replybuf = *rbuf;

  for(i=0;i<bufflen;i++)
    {
      temp = buf[i];
      if( 'a' <= temp && temp <= 'z'
	  || 'A' <= temp && temp <= 'Z'
	  || '0' <= temp && temp <= '9'
	  || temp == '-' || temp == '_' || temp == '.' )
	{
	  replybuf[j++] = temp;
	  /*	  printf("j -> %d\n", j); */
	}
      else
	{
	  replybuf[j++] = 37;
	  /* printf("temp -> %d\n", temp);
	     printf("d2h(temp >> 4) -> %d\n", d2h(temp >> 4));
	     printf("d2h(temp & 15) -> %d\n", d2h(temp & 15)); */
	  replybuf[j++] = d2h(temp >> 4);
	  replybuf[j++] = d2h(temp & 15);
	  /*	  printf("j -> %d\n", j); */
	}
    }
  return rlen_1;
}

/* Calculates the length of the resulting buffer if a string is URL encoded */
int calc_encoded_length(char* buf, int bufflen)
{
  unsigned int count=0, i=0, temp=0;

  for(i=0;i<bufflen;i++)
    {
      temp = buf[i];
      if( 'a' <= temp && temp <= 'z'
	  || 'A' <= temp && temp <= 'Z'
	  || '0' <= temp && temp <= '9'
	  || temp == '-' || temp == '_' || temp == '.' )
	{
	  count++;
	}
      else
	{
	  count = count+3;
	}
    }
  return count;
}

/* Converts an integer in the range 1-15 into it's ascii value
   1 -> 49 (ascii value of 1)
   10 -> 97 (ascii value of a)
*/
int d2h(unsigned int i)
{
  if( i < 10 )
    {
      return i + 48;
    }
  else
    {
      return i + 97 - 10;
    }
}
