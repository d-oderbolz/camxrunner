c**** RDOPTPA
c
      subroutine rdoptpa()
      use filunit
      use grid
      use procan
      use tracer
      implicit none
c  
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine loads all of the user options and flags for the
c     process analysis algorithm.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Argument description:      
c           none
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     06/08/00   --gwilson--    Original development
c     10/06/04   --cemery --    Restructured for namelist input
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'namelist.inc'
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer      igrd, nox, noy, n
      logical      lerror
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- if option is turned off, just return ---
c
      if( .NOT. lproca ) goto 9999
c
c   ---- Number of Process Analysis domains
c
      npadom = Number_of_PA_Domains
c
c   ---- Allocate the arrays that are dimensioned by grids ---
c
      if( npadom .LE. 0 ) goto 7004
      call alloc_procan(ngrid,ncol,nrow,nlay)
c
c   ---- PA domain definitions ---
c
      do n = 1,npadom
c
c   --- the grid number ---
c
         ipagrd(n) = Within_CAMx_Grid(n)
         if( ipagrd(n) .LE. 0 .OR. ipagrd(n) .GT. ngrid ) goto 7005
c
c   --- the cell indexes in the X adn Y directions
c
         i_sw(n) = PA_Beg_I_Index(n)
         i_ne(n) = PA_End_I_Index(n)
         j_sw(n) = PA_Beg_J_Index(n)
         j_ne(n) = PA_End_J_Index(n)
         b_lay(n) = PA_Beg_K_Index(n)
         t_lay(n) = PA_End_K_Index(n)
c
c   --- check for valid cell indexes ---
c
         lerror = .FALSE.
         igrd = ipagrd(n)
         if( igrd .GT. 1 ) then
            nox = (inst2(igrd) - inst1(igrd) + 1 ) * meshold(igrd) + 2
            noy = (jnst2(igrd) - jnst1(igrd) + 1 ) * meshold(igrd) + 2
         else
            nox = ncol(1)
            noy = nrow(1)
         endif
         if( i_sw(n) .LE. 0 .OR. i_sw(n) .GT. i_ne(n) ) lerror = .TRUE. 
         if( i_ne(n) .GT. nox ) lerror = .TRUE. 
         if( j_sw(n) .LE. 0 .OR. j_sw(n) .GT. j_ne(n) ) lerror = .TRUE. 
         if( j_ne(n) .GT. noy ) lerror = .TRUE. 
         if( b_lay(n) .LE. 0 .OR. b_lay(n) .GT. t_lay(n) ) 
     &                                                  lerror = .TRUE.
         if( t_lay(n) .GT. nlay(igrd) ) lerror = .TRUE.
         if( lerror ) goto 7006
      enddo
c
c  --- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7004 continue
      write(iout,'(//,a)') 'ERROR in RDOPTPA:'
      write(iout,'(1X,2A,I3)') 'Invalid value for number of ',
     &                   'Process Annalysis domains: ',npadom
      call camxerr()
c
 7005 continue
      write(iout,'(//,a)') 'ERROR in RDOPTPA:'
      write(iout,'(1X,2A,I3)') 'Invalid value for grid number for ',
     &                         'Process Analysis domain: ',ipagrd(n)
      call camxerr()
c
 7006 continue
      write(iout,'(//,a)') 'ERROR in RDOPTPA:'
      write(iout,'(1X,A,I3,A)') 'Cell indexes for PA sub-domain: ',n,
     &                                                 ' are invalid.'
      write(iout,'(10X,A,I3,10X,A)') 'Sub-Domain #',n,'Grid Definition'
      write(iout,'(14X,2A,18X,A,I3)') 'Beg ',' End','Grid #',igrd
      write(iout,'(A10,1X,2I5,20X,I5)') 'Rows    :',i_sw(n),i_ne(n),nox
      write(iout,'(A10,1X,2I5,20X,I5)') 'Columns :',j_sw(n),j_ne(n),noy
      write(iout,'(A10,1X,2I5,20X,I5)') 'Layers  :',
     &                                    b_lay(n),t_lay(n),nlay(igrd)
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
