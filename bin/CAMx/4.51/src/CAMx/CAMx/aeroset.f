      subroutine aeroset(dsec_i,ierr)
c
c----CAMx v4.51 080522
c
c     AEROSET prepares for the AERO chemistry routines in CAMx
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c  
c     Modifications:
c        12/29/06   --bkoo--     Added species mapping for the updated SOA scheme
c        01/10/07   --bkoo--     Included camx_aero.com
c                                Set pointers for aqueous PM
c
c     Input arguments: 
c        dsec_i              User supplied section cutpoints 
c 
c     Output arguments: 
c        ierr                Error flag
c            
c     Routines called: 
c        UPTIME
c            
c     Called by: 
c        READCHM
c 
      include 'camx.prm' 
      include 'camx.com'
      include 'grid.com'
      include 'chmstry.com' 
      include 'filunit.com'
      include 'section.inc'
      include 'section_aq.inc'
      include 'camx_aero.inc'
      include 'camx_aero.com'
c
      real*8  dsec_i(nsecp1)
c     
c-----Entry point 
c
      ierr = 0
      lfrst = .true.
c
c-----Reset dtaero to hit output interval exactly
c
      dtio = amin1(60.,dtinp,dtems,dtout)
      nsteps = INT( 0.999*dtio/dtaero ) + 1
      dt_aero = dtio/nsteps
      do n = 1,ngrid
        time_aero(n) = time
        date_aero(n) = date
        call uptime(time_aero(n),date_aero(n),60.*dt_aero)
        aero_dt(n)  = 0.0
      enddo
c
c-----Set pointers for aqueous PM
c
      if (idmech.EQ.4) then ! CB4
        khpo_c = kh2o2
        kfoa_c = nspec+1
        kmhp_c = nspec+1
        kpaa_c = nspec+1
        kohp_c = nspec+1
        kopa_c = nspec+1
      elseif (idmech.EQ.5) then ! SAPRC99
        khpo_c = kho2h
        kfoa_c = khc2h
        kmhp_c = kcooh
        kpaa_c = kco3h
        kohp_c = krooh
        kopa_c = krc3h
      elseif (idmech.EQ.6) then ! CB05
        khpo_c = kh2o2
        kfoa_c = kfacd
        kmhp_c = kmepx
        kpaa_c = kpacd
        kohp_c = krooh
        kopa_c = nspec+1
      endif
c
      if (aeropt .ne. 'CMU') return
c
c-----Check nbin against nsec parameters used in AERO modules
c
      if ( nbin .ne. nsec ) then
        write(iout,*) ' ERROR:  nbin and nsec must be equal!'
        write(iout,*) ' NBIN =',nbin, '; NSEC =',nsec
        write(iout,*) ' Set NSEC in SECTION.INC to ',nbin
        ierr = 1
        return
      endif
      if ( nsec .ne. nsect ) then
        write(iout,*) ' ERROR:  nsec and nsect must be equal!'
        write(iout,*) ' NSEC =',nsec, '; NSECT =',nsect
        write(iout,*) ' Set NSECT in SECTION_AQ.INC to ',nsec
        ierr = 1
        return
      endif
c
c-----The following specifies which AERO Modules to use 
c
      chaero = 'EQUI'
c      chaero = 'MADM'
c      chaero = 'HYBR'
      chaq = 'RADM'
c      chaq = 'VSRM' ! DO NOT USE THIS !!!
c      chaq = 'OVSR' ! DO NOT USE THIS !!!
c
c-----Set cwmin & tamin
c
      aqcwmin = cwmin
      aqtamin = tamin
c
c-----Calculate the initial diameters
c
c-----Check that user supplied section cutpoints are monotonically increasing
c
      do i = 2,nsecp1
        if ( dsec_i(i) .le. dsec_i(i-1) ) then
          write(iout,'(//,a)') 'ERROR in AEROSET:'
          write(iout,'(1X,2a)') 'Invalid section cut-points:',
     &                         ' they must be monotonically increasing!'
          write(iout,'(1X,a,/)') 'Input cut-points are :'
          do n = 1,nsecp1
            write(iout,'(4X,i2,1X,D9.3)') n,dsec_i(n)
          enddo
          call camxerr()
        endif
      enddo
c
c-----For the MOVING sectional approach dsec is the SECTIONAL DIAMETER
c
      dmin = dsec_i(1)
      dmax = dsec_i(nsecp1)
      do i = 1,nsecp1
        dsecf_c(i) = dsec_i(i)
      enddo

      write(idiag,*) ' '
      write(idiag,*) 'Particle section cut-points:'
      do i = 1,nsecp1
        write(idiag,'(1x,i2,1x,d9.3)') i,dsecf_c(i)
      enddo
      write(idiag,*) ' '
c
c-----Set moving diameters to logarithmic mean of fixed section diameters 
c     (tmg,01/25/02)
c
      do i = 1,nsec
        dsec_c(i) = sqrt(dsecf_c(i)*dsecf_c(i+1))
      enddo
c
c-----Additional pointers for RADM
c
      kso2_c   = kso2
      ko3_c    = ko3
      knxoy_c  = knxoy
      khno3_c  = khno3
      knh3_c   = knh3
      kh2so4_c = ksulf
      kpso4_c  = kpso4_1
      kpnh4_c  = kpnh4_1
      kpno3_c  = kpno3_1
      kna_c    = kna_1
      kpcl_c   = kpcl_1
c
c-----Additional pointers for SOAP
c
      kcg1_c   = kcg1
      kcg2_c   = kcg2
      kcg3_c   = kcg3
      kcg4_c   = kcg4
      kcg5_c   = kcg5
      kcg6_c   = kcg6
      kcg7_c   = kcg7
      ksoa1_c  = ksoa1_1
      ksoa2_c  = ksoa2_1
      ksoa3_c  = ksoa3_1
      ksoa4_c  = ksoa4_1
      ksoa5_c  = ksoa5_1
      ksoa6_c  = ksoa6_1
      ksoa7_c  = ksoa7_1
      ksopa_c  = ksopa_1
      ksopb_c  = ksopb_1
      kpoa_c   = kpoa_1
c
c-----Additional pointers for ISORROPIA
c
      khcl_c   = khcl
      kpec_c   = kpec_1
      kph2o_c  = kph2o_1
c
c-----Additional pointers for total PM
c
      kcrst_c  = kcrst_1
c
c-----Pointers for VSRM only
c
      kh2o2_c  = kh2o2
      kform_c  = kform
      khono_c  = khono
      koh_c    = koh
      kho2_c   = kho2
      kno3_c   = kno3
      kno_c    = kno
      kno2_c   = kno2
      kpan_c   = kpan
c
c-----Number of modeling species
c
      nspec_c  = nspec
c
      return
      end
