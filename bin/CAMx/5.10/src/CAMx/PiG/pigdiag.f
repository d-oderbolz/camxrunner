      subroutine pigdiag(idiag,chtime,chdate,igrd,note)
      use grid
      use pigsty
c
c----CAMx v5.10 090918
c
c-----PIGDIAG outputs key info on each puff
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
      include 'camx.prm'
      include 'flags.com'
c
      character*20 note
      character*8 chtime,chdate
c
      write(idiag,'(/,a,a8,1x,a8)') 'PiG puff info at : ',chtime,chdate
      write(idiag,'(a,i3)') 'Grid #: ',igrd
      write(idiag,'(a)') note
      write(idiag,'(2a)')
     & 'Puff Grid  Point   age       xf        xb        yf        yb ',
     & '     sy     sz     ax     ay     az   fmass  top   bot'
      write(idiag,'(2a)')
     & ' #     #    ID    (min)                 (km or deg)           ',
     & '             (meters)                         (meters)'
      write(idiag,'(2a)')
     & '-----  -- ------ ------- -------------------------------------',
     & '-- ----------------------------------  ---- -----------'
      do 100 n = 1,npig
c       if (ingrd(n).eq.0 .or. ingrd(n).ne.igrd) goto 100
        if (ingrd(n).eq.0) goto 100
        xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,i,j,idum)
        xdist = (xpigf(n) - xpigb(n))/
     &          delx*deltax(j,ingrd(n))*meshold(ingrd(n))
        ydist = (ypigf(n) - ypigb(n))/
     &          dely*deltay(ingrd(n))*meshold(ingrd(n))
        xlen  = sqrt(xdist**2 + ydist**2)     
        ay = axisy(n)
        az = axisz(n)
        ax = xlen + 3.*sigx(n)

        write(idiag,'(i5,i4,i7,f8.2,4f10.3,5f7.0,2x,f4.2,
     &                2f6.0)') 
     &                   n,ingrd(n),idpig(n),agepigf(n)/60.,xpigf(n),
     &                   xpigb(n),ypigf(n),ypigb(n),sigy(n),sigz(n),
     &                   ax,ay,az,fmspig(n),pufftop(n),puffbot(n)

cPuff Grid  Point   age       xf        xb        yf        yb      sy     sz     ax     ay     az   fmass  top   bot
c #     #    ID    (min)                 (km or deg)                        (meters)                         (meters)
c-----  -- ------ ------- --------------------------------------- ----------------------------------  ---- -----------
ciiiii  ii iiiiii ffff.ff -ffff.fff -ffff.fff -ffff.fff -ffff.fff fffff. fffff. fffff. fffff. fffff.  f.ff ffff. ffff.

 100  continue
      return
      end
