#include "util.h"

MPI_Request par_retrieve_tag(int fmsgtag)
{
   extern int flag_msgtag;
   extern MPI_Request mpi_msgtags[MAX_MSGTAG];

   MPI_Request i;

   /*For Safety*/
   if (fmsgtag >= MAX_MSGTAG) 
      printf("ERROR:par_retrieve_tag:fmsgtag >= MAX_MSGTAG\n");
    
   i = mpi_msgtags[fmsgtag];

   mpi_msgtags[fmsgtag] = 0;

   return (i);
}
