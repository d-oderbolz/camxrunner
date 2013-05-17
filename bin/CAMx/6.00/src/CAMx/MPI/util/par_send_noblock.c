#include "util.h"

void par_send_noblock_(int *mach, int *msgtype, int *fmsgtag)
{

   extern char *ibuff;
   extern int ipos, nbuff;

   MPI_Request msgtag;

   int ierr=0;

   if (flag_msgtag == 0) zero_tag_array();

   ierr= MPI_Isend( ibuff, ipos, MPI_PACKED, *mach, *msgtype, MPI_COMM_WORLD
    ,&msgtag);

   *fmsgtag=par_store_tag(msgtag);

   if(ierr < 0) 
    printf("Error in par_send_noblock - %d %d %d \n",*mach, *msgtype,ierr);

}
