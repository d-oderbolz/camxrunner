      subroutine hddmadj(lpost,nfam,con,crad,sddm,sraddm)
      use chmstry
      use tracer
c
c----CAMx v5.10 090918
c
c     HDDMADJ applies pre- or post-HDDMCHEM adjustments to the HDDM
c     sensitivities.
c
c     Copyright 1996-2009
c     ENVIRON International Corporation
c
c     History:
c        07/16/07   --bkoo--       Original development
c
c     Input arguments:
c        lpost             TRUE if this is post-HDDMCHEM adjustments
c        nfam              number of sensitivity families
c        con               state species concentrations (ppm)
c        crad              radical species concentrations (ppm)
c        sddm              sensitivity matrix sddm (ppm)
c        sraddm            radical sensitivity matrix sraddm (ppm)
c
c     Output arguments:
c        sddm              updated sensitivity matrix
c        sraddm            updated radical sensitivity matrix
c
c     Routines called:
c        None
c
c     Called by:
c        CHEMDRIV
c
      implicit none
      include 'camx.prm'

      logical  lpost
      integer  nfam
      real     con(MXSPEC+1),crad(MXRADCL)
      real     sddm(nfam,MXSPEC),sraddm(nfam,MXRADCL)

      real     FUZZ
      parameter ( FUZZ = 10.0 )

      integer  i,ip
      real     scl
c
c---  Entry point
c
      if ( lpost ) then
c
c---  Post-HDDMCHEM adjustments
c
c---  Set sensitivities of NXOY
c
        do ip = 1, nfam
          sddm(ip,kNXOY) = sraddm(ip,kNO3) + 2.0*sraddm(ip,kN2O5)
        enddo
c
c---  Reset sensitivities for small concentrations
c
        do i = 1, ngas
          if ( con(i) .LE. FUZZ*bdnl(i) ) then
            do ip = 1, nfam
              sddm(ip,i) = 0.0
            enddo
          endif
        enddo

      else
c
c---  Pre-HDDMCHEM adjustments
c
c---  Reset sensitivities for small concentrations
c
        do i = 1, ngas
          if ( con(i) .LE. FUZZ*bdnl(i) ) then
            do ip = 1, nfam
              sddm(ip,i) = 0.0
            enddo
          endif
        enddo
c
c---  Adjust sensitivities for the NXOY scheme
c
cbk        do ip = 1, nfam
cbk          scl = sraddm(ip,kNO3) + 2.0*sraddm(ip,kN2O5)
cbk          if (abs(scl).gt.1.e-25) then
cbk            scl = sddm(ip,kNXOY) / scl
cbk            sraddm(ip,kNO3)  = scl * sraddm(ip,kNO3)
cbk            sraddm(ip,kN2O5) = scl * sraddm(ip,kN2O5)
cbk          endif
cbk        enddo

      endif
c
c---  Return point
c
      return
      end
