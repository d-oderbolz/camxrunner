#include "util.h"

void par_assoc_buff_(buff,numbuff)
     int *numbuff;
     void *buff;
{
  extern char *ibuff;
  extern int ipos,nbuff;

  ibuff=buff;
  nbuff=*numbuff*sizeof(float);
  ipos=0;
}
