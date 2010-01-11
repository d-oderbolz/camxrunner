c**** RDOPTDDM
c
      subroutine rdoptddm
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine loads all of the user options and flags for the
c     source apportionment algorithm.  This version is for the
c     DDM technology type, which has different options.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Argument description:
c           none
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     03/23/99   --gwilson--    Original development
c     10/20/00   --cemery --    Split read of LSFCFL into 2 separate records
c     07/19/01   --gyarwood-    Initialize ngroup, nregin, lbndry
c     10/06/04   --cemery  -    Restructured for namelist input
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
      include 'filunit.com'
      include 'grid.com'
      include 'tracer.com'
      include 'procan.com'
      include 'namelist.com'
c
      integer i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- if option is turned off, just return ---
c
      if( .NOT. lddm ) goto 9999
c
c   --- flags for opening master and nest grid output surface files
c
      lsfcfl = DDM_Master_Sfc_Output
      if( ngrid .GT. 1 ) lsfcfl = DDM_Nested_Sfc_Output
c
c   --- get the number of Initial conditions species ---
c
      nicddm =  Number_of_IC_Species_Groups
      if( nicddm .GT. 0 ) then
         do i = 1,nicddm
           icddmsp(i) = IC_Species_Groups(i)
           call toupper( icddmsp(i) )
           call jstlft( icddmsp(i) )
         enddo
      endif
c
c   --- get the number of boundary conditions species ---
c
      lbndry = .FALSE.
      nbcddm = Number_of_BC_Species_Groups
      if( nbcddm .GT. 0 ) then
         do i = 1,nbcddm
           bcddmsp(i) = BC_Species_Groups(i)
           call toupper( bcddmsp(i) )
           call jstlft( bcddmsp(i) )
         enddo
c
c   --- flag for stratifying the boundary by edge ---
c
         lbndry = DDM_Stratify_Boundary
      endif
c
c   --- get the number of emissions species ---
c
      ngroup = 0
      nregin = 0
      nemddm = Number_of_EM_Species_Groups
      if( nemddm .GT. 0 ) then
         do i=1,nemddm
           emddmsp(i) = Emis_Species_Groups(i)
           call toupper( emddmsp(i) )
           call jstlft( emddmsp(i) )
         enddo
c
c   --- number of source regions ---
c
        nregin = DDM_Number_of_Source_Regions
        if( nregin .EQ. 0 ) then
           write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
           write(iout,'(/,1X,2A,/,A)') 'When requesting ',
     &                'emissions species for DDM you must ',
     &                'provide at least 1 emissions region.'
           call camxerr()
        endif
c
c   --- number of source emissions groupings ---
c
        ngroup = DDM_Number_of_Source_Groups
        if( ngroup .EQ. 0 ) then
           write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
           write(iout,'(/,1X,2A,/,A)') 'When requesting ',
     &                'emissions species for DDM you must ',
     &                'provide at least 1 emissions group.'
           call camxerr()
        endif
      endif
c
c   --- leftover is always false with DDM ---
c
      leftovr = .FALSE.
c
c   --- check for array overflow ---
c
      if( ngroup .GT. MXTEMF-1 ) then
         write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
         write(iout,'(/,1X,A,I4,A)') 
     &                  'Number of source groupings ',ngroup,
     &                  ' exceeds maximum.  Increase MXTEMF.'
         call camxerr()
      endif
      if( ngroup .LT. 0 ) then
         write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
         write(iout,'(1X,A,I4,A)') 
     &                  'Number of emissions groups ',ngroup,
     &                                         ' is invalid.'
         call camxerr()
      endif
c
c   --- number of timing realeases is always zero for DDM ---
c
      ntrtim = 0
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
