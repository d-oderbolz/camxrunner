#include "util.h"

void par_send_noblock_(mach,msgtype,msgtag)
     int *mach, *msgtype;
     MPI_Request *msgtag;
{
  extern char *ibuff;
  extern int ipos, nbuff;
  int ierr=0;
/*  int addr, nummach, mynum, msgid, nbytes;*/
    ierr= MPI_Isend( ibuff, ipos, MPI_PACKED, *mach, *msgtype, MPI_COMM_WORLD, 
                     msgtag);

  if(ierr < 0)
    printf("Error in par_send_noblock - %d %d %d \n",*mach, *msgtype,ierr);
}

