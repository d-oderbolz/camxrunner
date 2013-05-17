#include "util.h"

void par_put_float_(words,numwords)
     int *numwords;
     float *words;
{
  extern char *ibuff;
  extern int ipos, nbuff;
  int ierr=0;

  ierr = MPI_Pack( words,*numwords,MPI_FLOAT,ibuff,nbuff,&ipos
		  ,MPI_COMM_WORLD);

  if(ierr < 0) printf("Error in par_put_float- %d \n",ierr);
}
