/*
!
! Copyright (C) 1991-2004  ; All Rights Reserved ; ATMET, LLC
! 
! This file is free software; you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software 
! Foundation; either version 2 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with this 
! program; if not, write to the Free Software Foundation, Inc., 
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!======================================================================================
*/

#include <stdio.h>
#include "utils_sub_names.h"

#if defined (RAMS_MPI)
#include <mpi.h>
#define MAX_MSGTAG 1000

#else

#define MAX_MSGTAG 1
#define MPI_Request int
#endif

int flag_msgtag=0;
MPI_Request mpi_msgtags[MAX_MSGTAG];


char *ibuff;
int ipos,nbuff;

/*==========================================================================*/

void par_init_put(char *buff,int *numbuff) 
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos, nbuff;
#endif

#if defined (RAMS_MPI)
  /*ibuff= buff;*/
  ibuff=(char*)buff;
  nbuff=*numbuff*sizeof(float);
  ipos=0 ;
#endif

}

/*==========================================================================*/

void par_send(mach,msgtype)
     int *mach, *msgtype;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos, nbuff;
#endif
  int ierr=0;
/*  int addr, nummach, mynum, msgid;*/ 

#if defined (RAMS_MPI)
/*  MPI_Request msgreq;*/ 
/*  printf(" par_send- %d %d %d %d \n", *mach, *msgtype, ibuff,ipos);*/ 
  ierr= MPI_Send( ibuff, ipos, MPI_PACKED, *mach, *msgtype, MPI_COMM_WORLD);
/*  printf(" par_sent- %d %d %d %d %d \n", *mach, *msgtype, msgreq,ipos,ierr);*/
#endif


  if(ierr < 0) 
    printf("Error in par_send - %d %d %d \n",*mach, *msgtype,ierr);

 /* printf(" par_send- %d %d %d %d \n", *mach, *msgtype, ierr,ipos);*/
}

/*==========================================================================*/

void par_put_int(iwords,numwords)
     int *iwords, *numwords;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos,nbuff;
#endif
  int ierr=0, isize=0;

#if defined (RAMS_MPI)
  ierr = MPI_Pack( iwords,*numwords,MPI_INT,ibuff,nbuff,&ipos
		  ,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_put_int- %d %d %d \n",ierr,isize,ipos);
}

/*==========================================================================*/

void par_put_float(words,numwords)
     int *numwords;
     float *words;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos, nbuff;
#endif
  int ierr=0;

#if defined (RAMS_MPI)
  ierr = MPI_Pack( words,*numwords,MPI_FLOAT,ibuff,nbuff,&ipos
		  ,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_put_float- %d \n",ierr);
}

/*==========================================================================*/

void par_put_float8(words,numwords)
     int *numwords;
     float *words;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos, nbuff;
#endif
  int ierr=0;

#if defined (RAMS_MPI)
  ierr = MPI_Pack( words,*numwords,MPI_DOUBLE,ibuff,nbuff,&ipos
		  ,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_put_float- %d \n",ierr);
}

/*==========================================================================*/

void par_put_char(words,numbytes)
     int *numbytes;
     char *words;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos, nbuff;
#endif
  int ierr=0;

#if defined (RAMS_MPI)
  ierr = MPI_Pack( words,*numbytes,MPI_BYTE,ibuff,nbuff,&ipos
		  ,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_put_float- %d \n",ierr);
}

/*==========================================================================*/

/* Not sure how to do fortran logicals in C, Since they are 4 bytes in pgf90,
   use ints for now */
   
void par_put_log(words,numwords)
     int *numwords;
     int *words;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos, nbuff;
#endif
  int ierr=0;

#if defined (RAMS_MPI)
  ierr = MPI_Pack( words,*numwords,MPI_INT,ibuff,nbuff,&ipos
		  ,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_put_log- %d \n",ierr);
}
/* ========================================================= */

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

/* ========================================================= */

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

/* ========================================================= */

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



/*==========================================================================*/

void par_send_noblock(int *mach, int *msgtype, int *fmsgtag)
{

#if defined (RAMS_MPI)

   extern char *ibuff;
   extern int ipos, nbuff;

   MPI_Request msgtag;

   int ierr=0;

   /*   printf(" par_send- %d %d %d %d \n", *mach, *msgtype, ierr,ipos); */

   if (flag_msgtag == 0) zero_tag_array();

   ierr= MPI_Isend( ibuff, ipos, MPI_PACKED, *mach, *msgtype, MPI_COMM_WORLD
    ,&msgtag);

   *fmsgtag=par_store_tag(msgtag);

   /*  printf(" par_sent- %d %d %d %d \n", *mach, *msgtype, ierr,ipos);*/

   if(ierr < 0) 
    printf("Error in par_send_noblock - %d %d %d \n",*mach, *msgtype,ierr);

   /* printf(" par_send- %d %d %d %d \n", *mach, *msgtype, ierr,ipos);*/
#endif

}

/*==========================================================================*/

void par_get_noblock(void *buff,int *numbuff,int *mmtype
                               ,int *ihostnum,int *fmsgtag)
{
#if defined (RAMS_MPI)

   MPI_Request msgtag;
   int ierr=0, nbuff;


   if (flag_msgtag == 0) zero_tag_array();

   nbuff=*numbuff*sizeof(float);
   ipos=0;

   ierr=MPI_Irecv(buff,nbuff,MPI_PACKED,*ihostnum,*mmtype
                 ,MPI_COMM_WORLD,&msgtag);

   *fmsgtag=par_store_tag(msgtag);

   if(ierr < 0) 
    printf("Error in par_get_noblock\n");
#endif

}


/*==========================================================================*/

void par_assoc_buff(buff,numbuff)
     int *numbuff;
     void *buff;
{
  extern char *ibuff;
  extern int ipos,nbuff;

  ibuff=buff;
  nbuff=*numbuff*sizeof(float);
  ipos=0;
}

/*==========================================================================*/

void par_wait(int *fmsgtag,int *ibytes,int *msgtype,int *ihostnum)
{
#if defined (RAMS_MPI)
   
   MPI_Request msgtag;
   MPI_Status status;
   int ierr=0;
   
   /*  printf("Node waiting- %d %d %d %d\n",*msgtype,*ihostnum,*ibytes,mynum);*/

   if (flag_msgtag == 0) zero_tag_array();
   
   msgtag=par_retrieve_tag(*fmsgtag);
   
   ierr=MPI_Wait(&msgtag,&status);
  
   if(ierr < 0) 
      printf("Error in par_wait\n");
    
   MPI_Get_count(&status,MPI_PACKED,ibytes);
   *msgtype=status.MPI_TAG;
   *ihostnum=status.MPI_SOURCE;
   
   /*printf("Node done wait- %d %d %d %d\n",*msgtype,*ihostnum,*ibytes,mynum);*/

#endif


}

/*==========================================================================*/

void par_get_new(buff,numbuff,mmtype,ibytes,msgtype,ihostnum)
     int *numbuff,*mmtype,*ibytes,*msgtype,*ihostnum;
     void *buff;
{
  extern char *ibuff;
  extern int ipos,nbuff;
#if defined (RAMS_MPI)
  int ierr=0;
#endif
/*  int mpl_source,nummach,mynum,msgid;*/

#if defined (RAMS_MPI)
  MPI_Status status;
#endif

  ibuff=buff;
  nbuff=*numbuff*sizeof(float);
  ipos=0;

/* printf("Node waiting for- %d %d %d\n",*mmtype,*numbuff,nbuff);*/

#if defined (RAMS_MPI)
  ierr=MPI_Recv(ibuff,nbuff,MPI_PACKED,MPI_ANY_SOURCE,*mmtype
	   ,MPI_COMM_WORLD,&status);
      
  if(ierr < 0) 
    printf("Error in par_get_new\n");
    
  MPI_Get_count(&status,MPI_PACKED,ibytes);
  *msgtype=status.MPI_TAG;
  *ihostnum=status.MPI_SOURCE;
#endif

/* printf("Node got- %d %d %d %d\n",*msgtype,*ihostnum,*ibytes,mynum);*/

}

/*==========================================================================*/

void par_get_int(iwords,numwords)
     int *iwords,*numwords;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos,nbuff;
#endif
  int ierr=0;

/* printf("Unpack int- %d %d \n",*numwords,nbuff);*/
#if defined (RAMS_MPI)
  ierr = MPI_Unpack(ibuff,nbuff,&ipos,iwords,*numwords
	     ,MPI_INT,MPI_COMM_WORLD);
#endif

/* printf("Node got- %d %d %d %d\n",*msgtype,*ihostnum,*ibytes,mynum);*/
  if(ierr < 0) printf("Error in par_get_int-%d \n",ierr);
}

/*==========================================================================*/

void par_get_float(words,numwords)
     int *numwords;
     float *words;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos,nbuff;
#endif
  int ierr=0;

/* printf("Unpack flt- %d %d \n",*numwords,nbuff);*/
#if defined (RAMS_MPI)
  ierr= MPI_Unpack(ibuff,nbuff,&ipos,words,*numwords
		   ,MPI_FLOAT,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_get_float-%d \n",ierr);
}

/*==========================================================================*/

void par_get_float8(words,numwords)
     int *numwords;
     float *words;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos,nbuff;
#endif
  int ierr=0;

/* printf("Unpack flt8- %d %d \n",*numwords,nbuff);*/
#if defined (RAMS_MPI)
  ierr= MPI_Unpack(ibuff,nbuff,&ipos,words,*numwords
		   ,MPI_DOUBLE,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_get_float-%d \n",ierr);
}

/*==========================================================================*/

void par_get_char(words,numbytes)
     int *numbytes;
     char *words;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos,nbuff;
#endif
  int ierr=0;

/* printf("Unpack flt- %d %d \n",*numwords,nbuff);*/
#if defined (RAMS_MPI)
  ierr= MPI_Unpack(ibuff,nbuff,&ipos,words,*numbytes
		   ,MPI_BYTE,MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_get_float-%d \n",ierr);
}

/*==========================================================================*/

void par_get_log(iwords,numwords)
     int *iwords,*numwords;
{
#if defined (RAMS_MPI)
  extern char *ibuff;
  extern int ipos,nbuff;
#endif
  int ierr=0;

/* printf("Unpack int- %d %d \n",*numwords,nbuff);*/
#if defined (RAMS_MPI)
  ierr = MPI_Unpack(ibuff,nbuff,&ipos,iwords,*numwords
	     ,MPI_INT,MPI_COMM_WORLD);
#endif

/* printf("Node got- %d %d %d %d\n",*msgtype,*ihostnum,*ibytes,mynum);*/
  if(ierr < 0) printf("Error in par_get_log-%d \n",ierr);
}


/*==========================================================================*/
/*==========================================================================*/

void par_init_fortran (argc,fargv,farglen,machnum,machsize)
     int *argc,*farglen; char *fargv;
     int *machnum, *machsize;
{
  int i,numarg,carglen;
  char *argvp[20];
#if defined (RAMS_MPI)
  char **argv;
#endif
  
     numarg=*argc;
     carglen=*farglen;
    printf("par init numargs: %d %s %d %d\n",numarg,fargv,carglen,*machnum);

  for (i = 0; i < numarg; i++) {
    argvp[i]=&(fargv[i*carglen]);
    printf("par init args: %d %s %s\n",i,"argvp[i]",argvp[i]);
    }

#if defined (RAMS_MPI)
    printf("par init RAMS_MPI defined \n");
  argv=&(argvp[0]);
  MPI_Init(&numarg, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD,machnum);
  MPI_Comm_size(MPI_COMM_WORLD,machsize);
#endif

  printf("par_init: %d %d \n",*machnum,*machsize);
}

/*==========================================================================*/

void par_init(machnum,machsize)
     int *machnum, *machsize;
{
#if defined (RAMS_MPI)
     int argc; char **argv;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD,machnum);
  MPI_Comm_size(MPI_COMM_WORLD,machsize);
#endif

  printf("par_init: %d %d \n",*machnum,*machsize);
}

/*==========================================================================*/

void par_enroll(machnum)
     int *machnum;
{

}

/*==========================================================================*/

void par_exit()
{

#if defined (RAMS_MPI)
  MPI_Finalize();
#endif

  printf("MP exiting \n");

}

/*==========================================================================*/

void par_pause(machnum,ibarrier)
     int *machnum, *ibarrier;
{
  int ierr=0;

#if defined (RAMS_MPI)
  ierr = MPI_Barrier( MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_pause- %d %d %d \n"
          ,*machnum,*ibarrier,ierr);
}

/*==========================================================================*/

void par_ready(nmach,machnum,ibarrier)
     int *nmach,*machnum,*ibarrier;
{
  int ierr=0;

  printf("par_ready- %d %d \n",*ibarrier,*machnum);

#if defined (RAMS_MPI)
  MPI_Barrier( MPI_COMM_WORLD);
#endif

  if(ierr < 0) printf("Error in par_pause- %d %d %d \n"
          ,*machnum,*ibarrier,ierr);

}

