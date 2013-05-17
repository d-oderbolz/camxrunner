#include "util.h"

void par_init_put_(char *buff,int *numbuff) 
{
  extern char *ibuff;
  extern int ipos, nbuff;

  /*ibuff= buff;*/
  ibuff=(char*)buff;
  nbuff=*numbuff*sizeof(float);
  ipos=0 ;

}
