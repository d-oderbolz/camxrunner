#include "util.h"

void par_wait_(msgtag,ibytes,msgtype,ihostnum)
     int *ibytes,*msgtype,*ihostnum;
     MPI_Request *msgtag;
{
  MPI_Status status;
  int ierr=0;
/*  int mpl_source,nummach,mynum,msgid;*/

  ierr=MPI_Wait(msgtag,&status);
  if(ierr < 0)
    printf("Error in par_wait\n");
  MPI_Get_count(&status,MPI_PACKED,ibytes);
  *msgtype=status.MPI_TAG;
  *ihostnum=status.MPI_SOURCE;
}

