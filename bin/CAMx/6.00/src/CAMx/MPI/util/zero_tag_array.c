#include "util.h"

void zero_tag_array()
{
  extern int flag_msgtag;
  extern MPI_Request mpi_msgtags[MAX_MSGTAG];

  int i;
  
  flag_msgtag = 1;

  for(i=0;i<MAX_MSGTAG;i++)
    {
      mpi_msgtags[i] = 0;
    }

}
