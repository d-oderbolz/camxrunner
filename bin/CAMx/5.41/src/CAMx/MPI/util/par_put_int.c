#include "util.h"

void par_put_int_(iwords,numwords)
     int *iwords, *numwords;
{
  extern char *ibuff;
  extern int ipos,nbuff;
  int ierr=0, isize=0;

  ierr = MPI_Pack( iwords,*numwords,MPI_INT,ibuff,nbuff,&ipos
		  ,MPI_COMM_WORLD);

  if(ierr < 0) printf("Error in par_put_int- %d %d %d \n",ierr,isize,ipos);
}
