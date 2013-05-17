#include "util.h"

int par_store_tag(MPI_Request msgtag)
{
   extern int flag_msgtag;
   extern MPI_Request mpi_msgtags[MAX_MSGTAG];

   int i;

   for(i=0; i < MAX_MSGTAG; i++) {
      if (mpi_msgtags[i] == 0) {
         mpi_msgtags[i]= msgtag;
         return(i);
      }
   }

  printf("par_store_tag error:msgtag,max_msgtag= %d %d\n",msgtag,MAX_MSGTAG);

  return (i);
}
