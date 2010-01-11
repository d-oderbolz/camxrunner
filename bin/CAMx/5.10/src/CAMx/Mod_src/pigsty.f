      Module pigsty
      include 'pigsty.com'
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the LSBOX.COM
c        include file.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PIGSTY
c-----------------------------------------------------------------------
c
         subroutine alloc_pigsty(numrads,numspcs,numreact,numgrds,
     &                           numMach, PiGflag)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numrads    I  number of radicals
c        numreact   I  number of reactions
c        numspcs    I  number of species
c        numgrds    I  number of grids
c        numMach    I  number of processors
c        PiGflag    I  flag that determines if PiG is turned on
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numrads
         integer :: numreact
         integer :: numspcs
         integer :: numgrds
         integer :: numMach
         integer :: PiGflag
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( npigon(numgrds) )
         allocate( nage  (numgrds) )

         allocate( ingrd  (MXPIG) )
         allocate( idpig  (MXPIG) )
         allocate( xpigf  (MXPIG) )
         allocate( xpigb  (MXPIG) )
         allocate( ypigf  (MXPIG) )
         allocate( ypigb  (MXPIG) )
         allocate( zpig   (MXPIG) )
         allocate( axisy  (MXPIG) )
         allocate( axisz  (MXPIG) )
         allocate( sigy   (MXPIG) )
         allocate( sigx   (MXPIG) )
         allocate( sigz   (MXPIG) )
         allocate( htfms  (MXPIG) )
         allocate( htfmb  (MXPIG) )
         allocate( vtfms  (MXPIG) )
         allocate( vtfmb  (MXPIG) )
         allocate( agepigf(MXPIG) )
         allocate( agepigb(MXPIG) )
         allocate( fmspig (MXPIG) )
         allocate( lnewt  (MXPIG) )
         allocate( lnewg  (MXPIG) )
         allocate( pigage (MXGRID))
         allocate( ipufmap(MXPIG) )
         allocate( ipufgrp(MXPIG) )
c
         allocate( pufftop(MXPIG) )
         allocate( puffbot(MXPIG) )
c
         allocate( Lslice(numgrds, numMach, MXPIG) )
         allocate( puffrad(numrads,MAX(1,numreact),MXPIG) )
         allocate( puffmass(numspcs,MAX(1,numreact),MXPIG) )

         return

         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PIGSTY
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PIGSTY_SAMPLE
c-----------------------------------------------------------------------
c
         subroutine alloc_pigsty_sample()
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ismp1  (nsample) )
         allocate( ismp2  (nsample) )
         allocate( jsmp1  (nsample) )
         allocate( jsmp2  (nsample) )
         allocate( meshsmp(nsample) )
         allocate( ncolsmp(nsample) )
         allocate( nrowsmp(nsample) )
         allocate( ismpgrd(nsample) )
         allocate( ipsmp  (nsample) )
         allocate( xorgsmp(nsample) )
         allocate( yorgsmp(nsample) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PIGSTY_SAMPLE
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PIGSTY_SMPGRD
c-----------------------------------------------------------------------
c
         subroutine alloc_pigsty_smpgrd(numavspcs,numsamples,numcolsmp,
     &                                            numrowsmp,numsmpcells)
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numavspcs   I  number of species
c        numsamples I  number of sampling grids
c        numcolsmp  I  number of columns in each sampling grid
c        numrowsmp  I  number of rows in each sampliing grid
c     Output:  
c        numsmpcells I number of total sample cells 
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numavspcs
         integer :: numsamples
         integer :: numcolsmp(numsamples)
         integer :: numrowsmp(numsamples)
         integer :: numsmpcells
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvsmp2d
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvsmp2d = 0
         do i=1,numsamples
            mvsmp2d = mvsmp2d + numrowsmp(i) * numcolsmp(i)
         enddo
         mvecsmp = mvsmp2d * numavspcs
c
         allocate( smpcnc(mvecsmp) )
         numsmpcells = mvecsmp
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PIGSTY_SMPGRD
c-----------------------------------------------------------------------
c
      end Module
