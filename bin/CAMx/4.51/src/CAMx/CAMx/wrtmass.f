      subroutine wrtmass(igrd,chdate,chtime,ind)
c
c----CAMx v4.51 080522
c
c     WRTMASS writes out mass and fluxes at specified time,
c     and zeros out accumulation arrays
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c          
c     Modifications:
c        5/16/00   Modified mass output to facilitate import to a spreadsheet
c        2/02/06   Removed GREASD-PiG specific mass output
c
c     Input arguments:
c        igrd                grid index
c        chtime              simulation time ('HH:MM:SS')
c        chdate              simulation date ('YY/MM/DD')
c        ind                 code to select whether to output or initialize
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
      include "camxfld.com"
      include "chmstry.com"
      include "filunit.com"
c
      real*8 xincrmnt(MXSPEC),fluxin,fluxout
      real*8 pigdmp(MXSPEC)
      character*10 nammass(21)
      character*8 chdate,chtime
      data nammass/'Start Mass','Final Mass','Surf Emiss','Pnt Emiss ',
     &             'North IN  ','North OUT ','South IN  ','South OUT ',
     &             'East IN   ','East OUT  ','West IN   ','West OUT  ',
     &             'Top IN    ','Top OUT   ','Deposition','Chemistry ',
     &             'Nest Chnge','PiG Change','Net Change','Residual  ',
     &             'Mass Error'/
      data cf /1.e-6/
c
c-----Entry point
c
c-----Compute residual
c
      if (ind.eq.1) then
        do l = 1,nspec
          pigdmp(l) = pigdump(l,igrd)
        enddo
        do l = 1,nspec
          fluxin = 0.
          do i = 1,9,2
            fluxin = fluxin + fluxes(l+(i-1)*nspec,igrd) 
          enddo
          fluxout = 0.
          do i = 2,10,2
            fluxout = fluxout + fluxes(l+(i-1)*nspec,igrd) 
          enddo
          fluxout = fluxout + fluxes(l+(11-1)*nspec,igrd) 
          xincrmnt(l) = fluxin + fluxout + armass(l,igrd) + 
     &                  ptmass(l,igrd) + xmschem(l,igrd) + 
     &                  xmsfin(l,igrd) + pigdmp(l)
          resid(l,igrd) = xmsold(l,igrd) + xincrmnt(l) - xmass(l,igrd)
        enddo
c
c-----Write mass and fluxes at this date/hour
c
        write(imass,*)
        write(imass,'(a10,3x,a5,2a10,21(3x,a10))') 
     &       'Species','Grid','Date','Time',(nammass(i),i=1,21)
        do l = 1,nspec
          denom = max(xmsold(l,igrd), xmass(l,igrd),
     &            armass(l,igrd), ptmass(l,igrd),
     &            abs(xmschem(l,igrd)), abs(xmsfin(l,igrd)),
     &            abs(pigdmp(l)))
          do j = 1,11
            denom = max(abs(real(fluxes(l+(j-1)*nspec,igrd))),denom)
          enddo
          write(imass,'(3x,a10,i5,2x,a8,2x,a8,21(1pe13.4))') 
     &       spname(l),igrd,chdate,chtime,
     &       xmsold(l,igrd)*cf,xmass(l,igrd)*cf,armass(l,igrd)*cf,
     &       ptmass(l,igrd)*cf,(fluxes(l+(i-1)*nspec,igrd)*cf,i=1,11),
     &       xmschem(l,igrd)*cf,xmsfin(l,igrd)*cf,pigdmp(l)*cf,
     &       xincrmnt(l)*cf,resid(l,igrd)*cf,abs(resid(l,igrd)/denom)
        enddo
c
c-----Move current mass array to old mass array
c
        do l = 1,nspec
          xmsold(l,igrd) = xmass(l,igrd)
        enddo
      endif
c
c-----Zeros the mass and fluxes
c
      do l = 1,nspec
        armass(l,igrd) = 0.
        ptmass(l,igrd) = 0.
        do i=1,11
          fluxes(l+(i-1)*nspec,igrd) = 0.
        enddo
        xmschem(l,igrd) = 0.
        xmsfin(l,igrd) = 0.
        pigdmp(l) = 0.
        pigdump(l,igrd) = 0.
        pgmserr(l,igrd) = 0.
      enddo
c
      return
      end
