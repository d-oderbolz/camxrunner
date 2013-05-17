#include "util.h"

void par_get_int_(iwords,numwords)
     int *iwords,*numwords;
{
  extern char *ibuff;
  extern int ipos,nbuff;
  int ierr=0;

  ierr = MPI_Unpack(ibuff,nbuff,&ipos,iwords,*numwords
	     ,MPI_INT,MPI_COMM_WORLD);

  if(ierr < 0) printf("Error in par_get_int-%d \n",ierr);
}
