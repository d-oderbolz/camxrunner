c**** RDOPTDDM
c
      subroutine rdoptddm()
      use filunit
      use grid
      use procan
      use tracer
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine loads all of the user options and flags for the
c     source apportionment algorithm.  This version is for the
c     DDM technology type, which has different options.
c
c     Copyright 1996 - 2009
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
c     07/16/07   --bkoo--       Revised for HDDM
c     06/11/08   --bkoo--       Added rate constant sensitivity
c     07/16/08   --bkoo--       Added DDM turn-off flag
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'chmdat.com'
      include 'namelist.com'
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*200 strtmp
      integer i, j
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- if option is turned off, just return ---
c
      if( .NOT. lddm .AND. .NOT. lhddm ) goto 9999
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
c   --- number of rate constant sensitivity groups ---
c
      nrateddm = Number_of_Rate_Const_Groups
      allocate( rateddm(nrateddm) )
      allocate( iprate(0:nreact, nrateddm ) )
      allocate( ptwetfld(1) )
      do i=0,nreact
         do j=1,nrateddm
           iprate(i,j) = 0
         enddo
      enddo
      do i = 1, nrateddm
        strtmp = ADJUSTL(Rate_Const_Groups(i))
        j = INDEX(strtmp,':')
        if (j.eq.0) then
          write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
          write(iout,'(1X,A,I2,A)')
     &        'Delimiter (:) is not found in Rate_Const_Groups(',i,').'
          call camxerr()
        endif
        rateddm(i) = strtmp(:j-1)
        do
          strtmp = strtmp(j+1:)
          j = INDEX(strtmp,',')
          if (j.eq.0) then
            j = LEN_TRIM(strtmp) + 1
            if (j.eq.1) EXIT
          endif
          iprate(0,i) = iprate(0,i) + 1
          if (iprate(0,i).gt.MXREACT) then
            write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
            write(iout,'(1X,A,I2,A)')
     &            'Too many rxns (>MXREACT) in Rate_Const_Groups(',i,').'
            call camxerr()
          endif
          if ( VERIFY( TRIM( ADJUSTL( strtmp(:j-1) ) ), '1234567890' )
     &                                                    .ne. 0 ) then
            write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
            write(iout,'(1X,A,I2,A)')
     &                 'Invalid rxn index in Rate_Const_Groups(',i,').'
            call camxerr()
          endif
          read(strtmp(:j-1),'(i)') iprate(iprate(0,i),i)
        enddo
        if (iprate(0,i).eq.0) then
          write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
          write(iout,'(1X,A,I2,A)')
     &                   'No rxn is found in Rate_Const_Groups(',i,').'
          call camxerr()
        endif
      enddo
c
c   --- number of HDDM sensitivity groups ---
c
      nhddm = Number_of_HDDM_Sens_Groups
      if ( .NOT. lhddm .AND. nhddm .GT. 0 ) then
        write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
        write(iout,'(1X,A)')
     &              'Number_of_HDDM_Sens_Groups must be 0 if not HDDM.'
        call camxerr()
      endif
      allocate( hddmsp(2,nhddm) )
      do i = 1,nhddm
        do j = 1, 2
          hddmsp(j,i) = HDDM_parameters(i,j)
          call toupper( hddmsp(j,i) )
          call jstlft( hddmsp(j,i) )
        enddo
      enddo
c
c   --- set DDM turn-off flags ---
c
      allocate( lddmcalc(ngrid) )
      do i = 1, ngrid
        lddmcalc(i) = DDM_Calc_Grid(i)
        if ( lddmcalc(i) .OR. nbcddm.EQ.0 ) CYCLE
        write(iout,'(//,A)') 'ERROR in RDOPTDDM:'
        write(iout,'(1X,A)')
     &               'Flexi-DDM is not allowed with sensitivity to BC.'
        call camxerr()
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
