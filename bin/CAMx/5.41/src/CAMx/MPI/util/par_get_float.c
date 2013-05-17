#include "util.h"

void par_get_float_(words,numwords)
     int *numwords;
     float *words;
{
  extern char *ibuff;
  extern int ipos,nbuff;
  int ierr=0;

  ierr= MPI_Unpack(ibuff,nbuff,&ipos,words,*numwords
		   ,MPI_FLOAT,MPI_COMM_WORLD);

  if(ierr < 0) printf("Error in par_get_float-%d \n",ierr);
}
