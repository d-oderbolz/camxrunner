#include "util.h"

void par_get_noblock_(void *buff,int *numbuff,int *mmtype
                               ,int *ihostnum,int *fmsgtag)
{
   MPI_Request msgtag;
   int ierr=0, nbuff;

   if (flag_msgtag == 0) zero_tag_array();

   nbuff=*numbuff*sizeof(float);
   ipos=0;

   ierr=MPI_Irecv(buff,nbuff,MPI_PACKED,*ihostnum,*mmtype
                 ,MPI_COMM_WORLD,&msgtag);

   *fmsgtag=par_store_tag(msgtag);

   if(ierr < 0) 
    printf("Error in par_get_noblock\n");

}
