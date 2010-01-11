c**** SPECSA
c
      subroutine specsa(idate,begtim,jdate,endtim)
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets up the species names and pointers into the species
c   for all of the tracer species.  Pointers will be set up for both the
c   concentration array and the emissions array.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Argument descriptions:
c      Inputs:
c        idate  I   date of the beginning of the simulation (YYJJJ)
c        begtim R   hour of the begining of simulation
c        jdate  I   date of the ending of the simulation (YYJJJ)
c        endtim R   hour of the endng of simulation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     05/26/96   --gwilson--    Original development
c     12/12/97   --gwilson--    Fixed bug in initializing the timing
c                               tracers
c     11/06/01   --cemery--     Input dates are now Julian
c     
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'tracer.com'
      include 'filunit.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   idate
      integer   jdate
      real      begtim
      real      endtim     
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 name
      integer      ibegdt, ienddt
      integer      ncount, ioff, idtnow, nhours
      integer      i, j, k, l
      real         timnow, btim, etim
      logical      lgasflg(MXALCLS), lwrtcls(MXALCLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ibegdt = idate
      btim = begtim/100.
      ienddt = jdate
      etim = endtim/100.
c
c   --- set flags for wether this calss should be 
c       written to average file ---
c
      do i=1,MXALCLS
        lwrtcls(i) = .FALSE.
      enddo
      lwrtcls(ITRO3N) = .TRUE.
      lwrtcls(ITRO3V) = .TRUE.
      lwrtcls(ITRPS4) = .TRUE.
      lwrtcls(ITRPN3) = .TRUE.
      lwrtcls(ITRPN4) = .TRUE.
      lwrtcls(ITRPO1) = .TRUE.
      lwrtcls(ITRPO2) = .TRUE.
      lwrtcls(ITRPO3) = .TRUE.
      lwrtcls(ITRPO4) = .TRUE.
      lwrtcls(ITRPO5) = .TRUE.
      lwrtcls(ITRPEC) = .TRUE.
      lwrtcls(ITRPOA) = .TRUE.
      lwrtcls(ITRPFC) = .TRUE.
      lwrtcls(ITRPFN) = .TRUE.
      lwrtcls(ITRPCC) = .TRUE.
      lwrtcls(ITRPCS) = .TRUE.
      lwrtcls(ITRHG0) = .TRUE.
      lwrtcls(ITRHG2) = .TRUE.
      lwrtcls(ITRPHG) = .TRUE.
c
c  --- define the names of the tracer clasess ---
c
      clsnam(ITRNOX) = 'NOX'
      lgasflg(ITRNOX) = .TRUE.
c
      clsnam(ITRVOC) = 'VOC'
      lgasflg(ITRVOC) = .TRUE.
c
      clsnam(ITRO3N) = 'O3N'
      lgasflg(ITRO3N) = .TRUE.
c
      clsnam(ITRO3V) = 'O3V'
      lgasflg(ITRO3V) = .TRUE.
c
      clsnam(ITRSO2) = 'SO2'
      lgasflg(ITRSO2) = .TRUE.
c
      clsnam(ITRPS4) = 'PS4'
      lgasflg(ITRPS4) = .FALSE.
c
      clsnam(ITRRGN) = 'RGN'
      lgasflg(ITRRGN) = .TRUE.
c
      clsnam(ITRTPN) = 'TPN'
      lgasflg(ITRTPN) = .TRUE.
c
      clsnam(ITRNTR) = 'NTR'
      lgasflg(ITRNTR) = .TRUE.
c
      clsnam(ITRPN3) = 'PN3'
      lgasflg(ITRPN3) = .FALSE.
c
      clsnam(ITRHN3) = 'HN3'
      lgasflg(ITRHN3) = .TRUE.
c
      clsnam(ITRNH3) = 'NH3'
      lgasflg(ITRNH3) = .TRUE.
c
      clsnam(ITRPN4) = 'PN4'
      lgasflg(ITRPN4) = .FALSE.
c
      clsnam(ITRALK) = 'ALK'
      lgasflg(ITRALK) = .TRUE.
c
      clsnam(ITRARO) = 'ARO'
      lgasflg(ITRARO) = .TRUE.
c
      clsnam(ITRCRE) = 'CRE'
      lgasflg(ITRCRE) = .TRUE.
c
      clsnam(ITRTRP) = 'TRP'
      lgasflg(ITRTRP) = .TRUE.
c
      clsnam(ITRCG1) = 'CG1'
      lgasflg(ITRCG1) = .TRUE.
c
      clsnam(ITRCG2) = 'CG2'
      lgasflg(ITRCG2) = .TRUE.
c
      clsnam(ITRCG3) = 'CG3'
      lgasflg(ITRCG3) = .TRUE.
c
      clsnam(ITRCG4) = 'CG4'
      lgasflg(ITRCG4) = .TRUE.
c
      clsnam(ITRCG5) = 'CG5'
      lgasflg(ITRCG5) = .TRUE.
c
      clsnam(ITRPO1) = 'PO1'
      lgasflg(ITRPO1) = .FALSE.
c
      clsnam(ITRPO2) = 'PO2'
      lgasflg(ITRPO2) = .FALSE.
c
      clsnam(ITRPO3) = 'PO3'
      lgasflg(ITRPO3) = .FALSE.
c
      clsnam(ITRPO4) = 'PO4'
      lgasflg(ITRPO4) = .FALSE.
c
      clsnam(ITRPO5) = 'PO5'
      lgasflg(ITRPO5) = .FALSE.
c
      clsnam(ITRPEC) = 'PEC'
      lgasflg(ITRPEC) = .FALSE.
c
      clsnam(ITRPOA) = 'POA'
      lgasflg(ITRPOA) = .FALSE.
c
      clsnam(ITRPFC) = 'PFC'
      lgasflg(ITRPFC) = .FALSE.
c
      clsnam(ITRPFN) = 'PFN'
      lgasflg(ITRPFN) = .FALSE.
c
      clsnam(ITRPCC) = 'PCC'
      lgasflg(ITRPCC) = .FALSE.
c
      clsnam(ITRPCS) = 'PCS'
      lgasflg(ITRPCS) = .FALSE.
c
      clsnam(ITRHG0) = 'HG0'
      lgasflg(ITRHG0) = .TRUE.
c
      clsnam(ITRHG2) = 'HG2'
      lgasflg(ITRHG2) = .TRUE.
c
      clsnam(ITRPHG) = 'PHG'
      lgasflg(ITRPHG) = .FALSE.
c
c  --- calculate the beginning of the various tracer types ---
c      there will be (ngroup+1) if there is an extra group for the 
c      "leftover" group  ----
c
      if( lbndry ) then
          nbdic = 6 
      else
          nbdic = 2 
      endif
      if( ngroup .EQ. 0 ) then
          ncount = nregin
      else
          if( leftovr ) then
             ncount = (ngroup + 1) * nregin
          else
             ncount = ngroup * nregin
          endif
      endif
c
c   --- check for array overflow ---
c
      if( ntotsp .GT. MXTRSP ) goto 7000
c
c  --- set the flag for gaseous species ---
c
      do i=1,ntrcls
        do j=iptcls(i),nptcls(i)
           lsagas(j) = lgasflg(idxcls(i))
        enddo
      enddo
c
c   --- set the names for the initial condition tracers ---
c
      do i=1,ntrcls
         ptname(iptcls(i)) = clsnam(idxcls(i))//'000IC  '
      enddo
c
c   --- if stratifying by boundary there will be 5 boundary condition
c       tracers, otherwise there will be only one ---
c
      if( lbndry ) then
          do i=1,ntrcls
             ptname(iptcls(i) + IDXBNT) = clsnam(idxcls(i))//'NTHBC  '
             ptname(iptcls(i) + IDXBES) = clsnam(idxcls(i))//'ESTBC  '
             ptname(iptcls(i) + IDXBST) = clsnam(idxcls(i))//'STHBC  '
             ptname(iptcls(i) + IDXBWS) = clsnam(idxcls(i))//'WSTBC  '
             ptname(iptcls(i) + IDXBTP) = clsnam(idxcls(i))//'TOPBC  '
          enddo
      else
          do i=1,ntrcls
             ptname(iptcls(i)+1) = clsnam(idxcls(i))//'000BC  '
          enddo
      endif
c
c  --- construct the tracer names and put into names array ---
c
      if( ngroup .EQ. 0 ) then
          ioff = nbdic
          do i=1,nregin
             do k=1,ntrcls
                 write(name,'(A,I3.3,I3.3)') clsnam(idxcls(k)),1,i
                 ptname(iptcls(k)+ioff) = name
             enddo
             ioff = ioff + 1
          enddo
      else
          ioff = nbdic 
          if( leftovr ) then
             ncount = ngroup + 1
          else
             ncount = ngroup 
          endif
          do j=1,ncount
             do i=1,nregin
                do k=1,ntrcls
                   write(name,'(A,I3.3,I3.3)') clsnam(idxcls(k)),j,i
                   ptname(iptcls(k)+ioff) = name
                enddo
                ioff = ioff + 1
             enddo
          enddo
      endif
c
c  --- calculate the number of timing tracers there will be and put
c      the names into the names array ---
c
      if( ntrtim .GT. 0 ) then
        if( etim .EQ. 0. ) then
            etim = 24.
            ienddt = ienddt - 1
        endif 
        timnow = btim
        idtnow = ibegdt
        nhours = (ienddt-ibegdt)*24 + INT( etim - btim ) 
        npttim = 1
        do i=1,nhours
           if( MOD( INT(timnow), 24/ntrtim ) .EQ. 0 .OR. i .EQ. 1) then
              do j=1,nregin
                  write(name,'(A,I3.3,I2.2,I3.3)') 'I',MOD(idtnow,1000),
     &                                                  INT(timnow),j
                  if( ntotsp+1 .GT. MXTRSP ) goto 7001
                  ptname(ntotsp+1) = name
                  write(name,'(A,I3.3,I2.2,I3.3)') 'D',MOD(idtnow,1000),
     &                                                  INT(timnow),j
                  if( ntotsp+2 .GT. MXTRSP ) goto 7001
                  ptname(ntotsp+2) = name
                  npttim = npttim + 2
                  ntotsp = ntotsp + 2
              enddo
           endif
           timnow = timnow + 1.0
           if( timnow .EQ. 24.0 ) then
               timnow = 0.
               idtnow = idtnow + 1
           endif
        enddo
      endif
c
c   --- check for array overflow ---
c
      if( ntotsp .GT. MXTRSP ) goto 7000
c
c  --- initialize all of the tracers concs to zero to start off ---
c
      do i=1,MXSA3D
        ptconc(i) = 0.
      enddo
      do l=1,MXTRSP
         ptloft(l) = 0.
         lsamap(l) = l
         do i=1,MXRECP
            conrcp(l,i) = 0.        
         enddo
      enddo
c
c  --- set the flag for outputting the species to average file
c      to true automatically for tracer species ---
c
      if( lallout ) then
         do i=1,MXTRSP
            loutsa(i) = .TRUE.
         enddo
      else
         do i=1,MXTRSP
            loutsa(i) = .FALSE.
         enddo
         do j=1,ntrcls
           if( lwrtcls(idxcls(j)) ) then
              do i=iptcls(j),nptcls(j)
                 loutsa(i) = .TRUE.
              enddo
           endif
         enddo
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
      write(iout,'(//,a)') 'ERROR in SPECSA:'
      write(iout,'(/,1X,A,I5)') 
     &          'Number of tracer species exceeds max: ',MXTRSP
      write(iout,'(/,1X,A,I5)') 
     &         'You need room for at least this many tracers: ',ntotsp
      write(iout,'(1X,A)') 'Increase parameter MXTRSP in tracer.com'
      call camxerr()
c
 7001 continue 
      write(iout,'(//,a)') 'ERROR in SPECSA:'
      write(iout,'(/,1X,A,I5)') 
     &          'Number of tracer species exceeds max: ',MXTRSP
      write(iout,'(/,1X,A,I5)') 
     &                'You do not have room for the timing tracers.'
      write(iout,'(1X,A)') 'Increase parameter MXTRSP in tracer.com'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
