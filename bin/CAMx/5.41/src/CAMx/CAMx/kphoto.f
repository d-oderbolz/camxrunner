      subroutine kphoto(iozon,ialb,ihaze,hght,zenith,fcloud,cldtrns,
     &                  coefcld,ldark,ltuv,iabov,temp,pres)
      use chmstry
      implicit none
c 
c----CAMx v5.41 121109
c 
c     KPHOTO adjusts all photolysis rate constants for height, ozone
c     column, surface albedo, haze turbidity, and zenith angle
c                           
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c           
c     Modifications: 
c        4/2/03        Removed option for UAM-V type cloud adjustment
c       10/14/04       Modifications to handle Mechanism 10 (no photo rates)
c        7/14/10       Added code for in-line TUV cloud adjustment
c        4/12/12       Added T and P adjustments to photolysis rates
c 
c     Input arguments: 
c        iozon               ozone column index
c        ialb                surface albedo index
c        ihaze               haze turbidity index
c        hght                height AGL (km)
c        zenith              solar zenith angle (deg)
c        fcloud              cloud coverage
c        cldtrns             enery transmission coefficient
c        ldark               darkness flag (T=dark)
c        ltuv                In-line TUV flag
c        iabov               Flag to note above cloud (1=above)
c        temp                temperature (K)
c        pres                pressure (mb)
c 
c     Output arguments: 
c        coefcld             photolysis rate adjustment factor
c 
c     Routines Called: 
c        none
c 
c     Called by: 
c        CHEMDRIV
c        PIGDRIVE
c
      include "camx.prm"
c
c-----Arguments
c
      integer iozon,ialb,ihaze,iabov
      real hght,zenith,fcloud,cldtrns,coefcld
      real temp,pres
      logical ldark,ltuv
c
c-----Local
c
      integer nphot,irxn,ihght,izen
      integer i
      real zenang,cldrat,tmp,w1,w2
      real prkn11,prkn12,prkn21,prkn22,tmp1,tmp2
      real deg2rad
      real tref,pref,adj1,adj2
      real no2tadj(NZEN),o3tadj(NZEN),hchotadj1(NZEN),hchotadj2(NZEN),
     &     hchopadj(NZEN),ch3chopadj(NZEN)
c
      data deg2rad /0.01745329/
      data tref,pref /298.,1013./
c
c-----Photolysis T and P factors (%/K, %/mb) for 10 zenith angles from TUV
c
      data no2tadj
     &     /0.071,0.072,0.072,0.072,0.072,0.073,0.074,0.076,0.080,0.081/
      data o3tadj
     &     /0.265,0.268,0.275,0.288,0.308,0.339,0.384,0.447,0.485,0.454/
      data hchotadj1
     &     /0.024,0.024,0.024,0.025,0.026,0.026,0.028,0.030,0.031,0.032/
      data hchotadj2
     &     /0.033,0.033,0.033,0.033,0.033,0.034,0.034,0.036,0.036,0.038/
      data hchopadj
     &     /-.017,-.017,-.017,-.017,-.018,-.018,-.019,-.020,-.021,-.023/
      data ch3chopadj
     &     /-1.393E-03,-1.396E-03,-1.402E-03,-1.412E-03,-1.427E-03,
     &      -1.446E-03,-1.473E-03,-1.504E-03,-1.528E-03,-1.536E-03/
c
c-----Entry point
c
c-----If nphot is zero, nothing to do
c
      nphot = nphot1 + nphot2
      if (nphot.eq.0) return
c
c-----If dark, zero all the photolysis reaction rate constants
c
      if (ldark) then
        do irxn = 1,nphot1
          rk(idphot1(irxn)) = 0.
        enddo
        do irxn = 1,nphot2
          rk(idphot2(irxn)) = 0.
        enddo
        goto 999
      endif       
c
c-----Locate the point in height axis
c
      ihght = 1
      do i = 1,NHGHT-1
        if (hght.ge.htint(i)) ihght = i
      enddo
      tmp = (hght - htint(ihght))/(htint(ihght+1) - htint(ihght))
      w1 = 1. - tmp
c
c-----If height greater than uppermost height in the table
c
      if (tmp.gt.1.) then
        ihght = NHGHT - 1
        w1 = 0.
      endif
c
c-----Locate the point in zenith angle axis
c
      izen = 1
      do i = 1,NZEN-1
        if (zenith.ge.zenint(i)) izen = i
      enddo
      tmp = (zenith - zenint(izen))/(zenint(izen+1) - zenint(izen))
      w2 = 1. - tmp
c
c-----If zenith is greater than the uppermost zenith in the table
c
      if (tmp.gt.1.) then
        izen = NZEN-1
        w2 = 0.
      endif
c
c-----Set the primary photolysis rates using interpolation
c     in height and zenith angle
c
      do irxn = 1,nphot1
        prkn11 = prkn(izen,  irxn,ihght,  ihaze,ialb,iozon)
        prkn12 = prkn(izen,  irxn,ihght+1,ihaze,ialb,iozon)
        prkn21 = prkn(izen+1,irxn,ihght,  ihaze,ialb,iozon)
        prkn22 = prkn(izen+1,irxn,ihght+1,ihaze,ialb,iozon)
        tmp1 = w1*prkn11 + (1.-w1)*prkn12
        tmp2 = w1*prkn21 + (1.-w1)*prkn22
        rk(idphot1(irxn)) = w2*tmp1 + (1.-w2)*tmp2
      enddo
c
c-----Cloud coverage adjustment
c
      if (ltuv) then
        coefcld = cldtrns
      else
        zenang = amin1(zenith,60.0)
        zenang = deg2rad*zenang
        if (iabov.eq.1) then
          cldrat = 1. + (1. - cldtrns)*cos(zenang)
        else
          cldrat = 1.6*cldtrns*cos(zenang)
        endif
        coefcld = 1. + fcloud*(cldrat - 1.)
      endif
c
      do irxn = 1,nphot1
        rk(idphot1(irxn)) = rk(idphot1(irxn))*coefcld
      enddo
c
c-----Set secondary photolysis rates as ratios to primary rates
c
      do irxn = 1,nphot2
        rk(idphot2(irxn)) = rk(idphot3(irxn))*phtscl(irxn)
      enddo
c
c-----Apply T and P adjustments for current conditions
c
      adj1 = 1. + no2tadj(izen)*(temp - tref)/100.
      adj2 = 1. + no2tadj(izen+1)*(temp - tref)/100.
      rk(jno2rxn) = rk(jno2rxn)*(w2*adj1 + (1.-w2)*adj2)

      adj1 = 1. + o3tadj(izen)*(temp - tref)/100.
      adj2 = 1. + o3tadj(izen+1)*(temp - tref)/100.
      rk(jo3rxn) = rk(jo3rxn)*(w2*adj1 + (1.-w2)*adj2)

      adj1 = 1. + hchotadj1(izen)*(temp - tref)/100.
      adj2 = 1. + hchotadj1(izen+1)*(temp - tref)/100.
      rk(jhcho1rxn) = rk(jhcho1rxn)*(w2*adj1 + (1.-w2)*adj2)

      adj1 = 1. + hchotadj2(izen)*(temp - tref)/100.
     &           *hchopadj(izen)*(pres*tref/temp  - pref)/100.
      adj2 = 1. + hchotadj2(izen+1)*(temp - tref)/100.
     &           *hchopadj(izen+1) *(pres*tref/temp  - pref)/100.
      rk(jhcho2rxn) = rk(jhcho2rxn)*(w2*adj1 + (1.-w2)*adj2)

      adj1 = 1. + ch3chopadj(izen)*(pres*tref/temp  - pref)/100.
      adj2 = 1. + ch3chopadj(izen+1)*(pres*tref/temp  - pref)/100.
      rk(jch3chorxn) = rk(jch3chorxn)*(w2*adj1 + (1.-w2)*adj2)
c
 999  return
      end
