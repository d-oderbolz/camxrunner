      subroutine hddmadj(lpost,nfam,con,sddm)
      use chmstry
      use tracer
c
c----CAMx v5.41 121109
c
c     HDDMADJ applies pre- or post-HDDMCHEM adjustments to the HDDM
c     sensitivities.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     History:
c        07/16/07   --bkoo--       Original development
c
c     Input arguments:
c        lpost             TRUE if this is post-HDDMCHEM adjustments
c        nfam              number of sensitivity families
c        con               state species concentrations (ppm)
c        sddm              sensitivity matrix sddm (ppm)
c
c     Output arguments:
c        sddm              updated sensitivity matrix
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
      real     con(MXSPEC+1)
      real     sddm(nfam,MXSPEC)

      real     FUZZ
      parameter ( FUZZ = 10.0 )

      integer  i,ip
c
c---  Entry point
c
      if ( lpost ) then
c
c---  Post-HDDMCHEM adjustments
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

      endif
c
c---  Return point
c
      return
      end
