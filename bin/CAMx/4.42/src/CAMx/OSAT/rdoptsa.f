c**** RDOPTSA
c
      subroutine rdoptsa
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine loads all of the user options and flags for the
c     source apportionment algorithm.  
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
c     05/16/96   --gwilson--    Original development
c     02/10/97   --cemery--     Added read of fileroot for SA output files
c     04/14/97   --gwilson--    Changed the way number of source groups
c                               is specified
c     04/28/97   --gwilson--    Added flag for OPPAT
c     05/22/97   --gwilson--    Added flag for APCA
c     10/20/00   --cemery --    Split read of LSFCFL into 2 separate records
c     01/10/02   --cemery --    Eliminated the read for fine grid
c                               flag if no nests 
c     03/21/03   --gwilson--    Removed the OSAT technology type OPPAT
c     10/06/04   --cemery --    Restructured for namelist input
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
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer nemiss, ncount
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c
c   --- if option is turned off, just return ---
c
      if( .NOT. ltrace ) goto 9999
c
c   --- flags for opening master and nest grid output surface files
c
      lsfcfl = SA_Master_Sfc_Output
      if( ngrid .GT. 1 ) lsfcfl = SA_Nested_Sfc_Output
c
c   --- flag for stratifying the boundary by edge ---
c
      lbndry = SA_Stratify_Boundary
c
c   --- number of source regions ---
c
      nregin = SA_Number_of_Source_Regions
c
c   --- number of source emissions groupings ---
c
      nemiss = SA_Number_of_Source_Groups
      if( tectyp .EQ. GOAT .AND. nemiss .NE. 1 ) goto 7000
c
c   --- flag for determining if the leftover group should be used ---
c
      leftovr = Use_Leftover_Group
c
c   --- flag for determining what type of output to produce ----
c
      lallout = (.NOT. SA_Summary_Output )
c
c   --- set the number of source groups from the emissions groups ---
c
      if( nemiss .EQ. 1 .AND. leftovr ) then
         write(iout,'(//,a)') 'ERROR in RDOPTSA:'
         write(iout,'(/,1X,2A)') 'Cannot have leftover group ',
     &                           'with only one source group.'
         write(iout,'(1X,2A)') 'Set number of groups to 2 or turn ',
     &                         'off leftover group.'
         call camxerr()
      endif
      if( leftovr .OR. nemiss .EQ. 1 ) then
          ngroup = nemiss - 1
      else
          ngroup = nemiss 
      endif
c
c   --- Need the biogenics group if doing APCA ---
c
      if( tectyp .EQ. APCA .AND. ngroup .EQ. 0 ) then
         write(iout,'(//,a)') 'ERROR in RDOPTSA:'
         write(iout,'(/,1X,3A)')'Need biogenic sources as a seperate ',
     &                          'emissions group when doing APCA.'
         call camxerr()
      endif
c
c   --- check for array overflow ---
c
      if( ngroup .GT. MXTEMF-1 ) then
         write(iout,'(//,a)') 'ERROR in RDOPTSA:'
         write(iout,'(/,1X,A,I4,A)')'Number of source groupings ',
     &                ngroup,' exceeds maximum.  Increase MXTEMF.'
         call camxerr()
      endif
      if( ngroup .LT. 0 ) then
         write(iout,'(//,a)') 'ERROR in RDOPTSA:'
         write(iout,'(1X,A,I4,A)')'Number of emissions groups ',nemiss,
     &                            ' is invalid.'
         call camxerr()
      endif
c
c   --- number of timing releases per day ----
c
      ntrtim = Number_of_Timing_Releases
c
c   --- if doing PSAT, get the classes that should be treat ---
c	
      if( tectyp .EQ. PSAT ) then
         ncount = 0
         lsulfate = .FALSE.
         lnitrate = .FALSE.
         lsoa = .FALSE.
         lprimary = .FALSE.
         lmercury = .FALSE.
         if( PSAT_Treat_SULFATE_Class ) then
            lsulfate = .TRUE.
            ncount = ncount + 1
         endif
         if( PSAT_Treat_NITRATE_Class ) then
            lnitrate = .TRUE.
            ncount = ncount + 1
         endif
         if( PSAT_Treat_SOA_Class ) then
            lsoa = .TRUE.
            ncount = ncount + 1
         endif
         if( PSAT_Treat_PRIMARY_Class ) then
            lprimary = .TRUE.
            ncount = ncount + 1
         endif
         if( PSAT_Treat_MERCURY_Class ) then
            lmercury = .TRUE.
            ncount = ncount + 1
         endif
         if( PSAT_Treat_OZONE_Class ) then
            lozone = .TRUE.
            ncount = ncount + 1
         endif
         if( ncount .LE. 0 ) goto 7001
      endif
c
c  --- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in RDOPTSA:'
      write(iout,'(/,1X,2A)') 'The technology type option ',
     &               ' GOAT requires that there be 1 emissions group.'
      write(iout,'(10X,A,I3,A)') 'You supplied: ',nemiss,
     &                                            ' emissions groups.' 
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in RDOPTSA:'
      write(iout,'(/,1X,2A)') 'The PSAT technology type was ',
     &     'specified but no classes are selected for treatment.'
      write(iout,'(1X,2A)') 'Please activate at least one PSAT class ',
     &                          'in the CAMx control file.'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
