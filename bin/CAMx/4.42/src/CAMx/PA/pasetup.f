      subroutine pasetup
c
c----CAMx v4.42 070603
c
c     This routine initializes some of the data strucutures for the
c     Process Analysis algorithm.  The "species" names array is filled 
c     and the arrays are checked to make sure they have been allocated 
c     properly.  The pointers into the gridded arrays are initialized.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        none
c
c     Subroutines Called:
c        CPAMECH
c
c     Called by:
c        STARTUP
c
      implicit none
c
      include 'camx.prm'
      include 'chmstry.com'
      include 'grid.com'
      include 'tracer.com'
      include 'filunit.com'
      include 'procan.com'
c
c-----Argument declarations
c
c-----Local variables
c
      real rdum(MXRXN), rkdum(MXRXN), padum(MXCPA)  
      real dtfact
      integer i, ntmp, npa_init
c
c-----Entry point
c
c  --- IRR is not allowed with the LSODE solver ---
c
      if( lirr .AND. idsolv .EQ. IDLSOD ) goto 7002
c
c  --- set flag to determine whether CPA variables are cumulative ---
c
      lcpacum = .false.
c
c  --- make sure the parameters are defined properly ---
c
      ntmp = MXCPA
      if( MXTRSP .LT. ntmp ) goto 7000
c            
c  --- initialize the number of reactions ---
c
      nirrrxn =  nreact
c
c  --- set the number of species and initialize species names ---
c
c   --- dummy array set to zero for first call ---
c
      do i=1,MXRXN
       rdum(i) = 0.0
       rkdum(i) = 0.0
      enddo
      dtfact = 0.
c
c   --- dummy call sets number of CPA outputs and defines 
c       variable output names ---
c
      if(idmech .EQ. 3) then
         call cpamech3(rdum,rkdum,dtfact,MXRXN,padum,MXCPA,
     &                                           npa_init,.false.)
      elseif(idmech .EQ. 4) then
         call cpamech4(rdum,rkdum,dtfact,MXRXN,padum,MXCPA,
     &                                           npa_init,.false.)
      elseif(idmech .EQ. 5) then
         call cpamech5(rdum,rkdum,dtfact,MXRXN,padum,MXCPA,
     &                                           npa_init,.false.)
      else
          goto 7001
      endif
c
c  --- add the cloud adjustment to photolysis rates ---
c
      npa_init = npa_init + 1
      ptname(npa_init)  = 'J_CLDADJ'
c     
c  --- set names of radical concentrations to be saved by CPA 
c      don't save if CPA is set to accumulate values ---
c       
      if(.NOT.lcpacum)
     &     call cparad(rdum, nrad, padum, MXCPA, npa_init, 0.0)
c
      ntotsp = npa_init
      nsaspc = npa_init
c
c  --- define the pointers for each grid ---
c
      ipsa3d(1) = 1
      do i=2,ngrid
         ipsa3d(i) = ipsa3d(i-1) + ncol(i-1)*nrow(i-1)*nlay(i-1)*MXCPA
      enddo
c
c   --- set the output flags for all species ----
c
      do i=1,MXCPA
        loutsa(i) = .TRUE.
      enddo
      lfirst = .TRUE.
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in PASETUP:'
      write(iout,*) 'Parameter for dimensioning vectors for',
     &                 ' tracer speices is not large enough.'
      write(iout,*) 'Make sure the parameter MXTRSP is at least ',
     &                 'as large as: ',MXCPA
      call camxerr()
      goto 9999
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in PASETUP:'
      write(iout,'(2A,I2)') 'Chemical Process Analysis (CPA) does ',
     &                       'not support chemical mechanism: ',idmech
      write(iout,*) 'Please choose another mechanism and try again.'
      call camxerr()
      goto 9999
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in PASETUP:'
      write(iout,'(2A)') 'Chemical Process Analysis (CPA) does ',
     &              'not support support the LSODE chemistry solver.'
      write(iout,*) 'Please choose another solver and try again.'
      call camxerr()
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
