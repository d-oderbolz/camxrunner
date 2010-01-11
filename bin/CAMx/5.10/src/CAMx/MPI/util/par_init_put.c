#include "util.h"

void par_init_put_(buff,numbuff)
/*     int *numbuff,buff; */
  int *numbuff;
  long buff;
{
  extern char *ibuff;
  extern int ipos, nbuff;

  ibuff=(char*)buff;

  nbuff=(*numbuff)*sizeof(float);
  //printf("initialized buffer size: %ld\n", *numbuff);
  ipos=0 ;
  return;
}

