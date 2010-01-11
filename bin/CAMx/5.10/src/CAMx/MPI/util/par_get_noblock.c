#include "util.h"

void par_get_noblock_(buff,numbuff,mmtype,ihostnum,msgtag)
     int *numbuff,*mmtype,*ihostnum;
     void *buff;
     MPI_Request *msgtag;
{
  extern char *ibuff;
  extern int ipos,nbuff;
  int ierr=0;

/*  MPI_Status status;*/
  ibuff=buff;
  nbuff=*numbuff*sizeof(float);
  ipos=0;

  ierr=MPI_Irecv(ibuff,nbuff,MPI_PACKED,*ihostnum,*mmtype
           ,MPI_COMM_WORLD,msgtag);
  if(ierr < 0)
    printf("Error in par_get_noblock\n");
}

