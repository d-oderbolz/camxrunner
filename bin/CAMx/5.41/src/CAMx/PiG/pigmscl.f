      subroutine pigmscl(nspcs,ngrid,chtime,chdate,idiag,pigdump,pgmserr)
      use pigsty
      use chmstry
      implicit none
c
c----CAMx v5.41 121109
c
c     PIGMSCL calculates total mass in all PiG puffs by grid
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        02/02/06            Renamed PIGMSCL from IRONMSCL
c        03/31/08            Added average puff age when killed
c
c     Input arguments:
c        nspcs               number of species
c        ngrid               number of grids
c        chtime              model time ('HH:MM:SS')
c        chdate              model date ('YY/MM/DD')
c        idiag               Diagnostic file unit number
c        pigdump             PiG dumped mass (umol)
c        pgmserr             error in PiG dumped mass (umol)
c
c     Output arguments:
c        none
c
c     Subroutines called:
c        none
c
c     Called by:
c        CAMx
c
      include "camx.prm"
c
      integer nspcs, ngrid,idiag,igrd,l,n,nr,is
      real*8  pigmass(nspcs,ngrid)
      real*8  pigdump(nspcs,ngrid)
      real*8  pgmserr(nspcs,ngrid)

      real*8 relerr
      character*8 chdate,chtime
c
c-----Entry point
c
      do igrd = 1,ngrid
        npigon(igrd) = 0
        do l = 1,nspec
          pigmass(l,igrd) = 0.
        enddo
      enddo
c
      do n = 1,npig
        igrd = ingrd(n)
        if (igrd.ne.0) then
          npigon(igrd) = npigon(igrd) + 1
          do nr = 1,nreactr
            do l = 1,nspec
              pigmass(l,igrd) =  pigmass(l,igrd) + puffmass(l,nr,n)
            enddo
          enddo
        endif
      enddo
c
      write(idiag,'(/,a,2x,a8,2x,a8)')
     &                      'PiG diagnostics at: ',chtime,chdate
      write(idiag,'(a,i5)') '# puffs killed by size      : ',nkill(1)
      write(idiag,'(a,i5)') '# puffs killed by mass/age  : ',nkill(2)
      write(idiag,'(a,i5)') '# puffs killed by bgnd LSODE: ',nkill(3)
      write(idiag,'(a,i5)') '# puffs killed by puff LSODE: ',nkill(4)
      write(idiag,'(a,i5)') '# puffs killed by stage 3   : ',nkill(5)
      write(idiag,'(a,i5)') '# puffs killed by bgnd AQPM : ',nkill(6)
      write(idiag,'(a,i5)') '# puffs killed by puff AQPM : ',nkill(7)
      write(idiag,'(a,i5)') '# puffs killed by bgnd PM   : ',nkill(8)
      write(idiag,'(a,i5)') '# puffs killed by puff PM   : ',nkill(9)

      do igrd = 1,ngrid 
        write(idiag,'(a,i5,a,i5)') '# active puffs in grid: ',igrd, 
     &                             ' is ',npigon(igrd) 
        write(idiag,'(a,i5,a,i5)') '# puffs killed in grid: ',igrd, 
     &                             ' is ',nage(igrd) 
        if (nage(igrd).gt.0) then
          write(idiag,'(a,f5.0,a)') 'Average puff age when killed is: ',
     &                            pigage(igrd)/nage(igrd)/60.,' minutes'
        endif
        write(idiag,'(a,a)')
     &  'PiG mass:    Total (mol)  Dumped (mol)   Error (mol)',
     &  '     Error (%)'
        nage(igrd) = 0
        pigage(igrd) = 0.

        do is = 1,nspec
          if (abs(pigdump(is,igrd)).gt.0.) then
            relerr = 100.*pgmserr(is,igrd)/abs(pigdump(is,igrd))
          else
            relerr = 0.
          endif
          write(idiag,'(a,4(4x,1pe10.3))')
     &       spname(is),pigmass(is,igrd)/1.e6,pigdump(is,igrd)/1.e6,
     &       pgmserr(is,igrd)/1.e6,relerr
        enddo
      enddo 

      nkill(1) = 0
      nkill(2) = 0
      nkill(3) = 0
      nkill(4) = 0
      nkill(5) = 0
      nkill(6) = 0
      nkill(7) = 0
      nkill(8) = 0
      nkill(9) = 0
c
      return
      end
