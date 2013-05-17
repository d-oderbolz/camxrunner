      subroutine kphoto(iozon,alb,terrain,hght,zenith,cldtrns,ldark,
     &                  temp,pres)
      use chmstry
      use o3colmap
      implicit none
c 
c----CAMx v6.00 130506
c 
c     KPHOTO adjusts all photolysis rate constants for height, ozone
c     column, surface albedo, terrain ht, and zenith angle
c                           
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        4/2/03        Removed option for UAM-V type cloud adjustment
c       10/14/04       Modifications to handle Mechanism 10 (no photo rates)
c        7/14/10       Added code for in-line TUV cloud adjustment
c        04/02/12      Removed RADM cloud adjustment option, cloud/aerosol
c                      adjustments now always done with in-line TUV; AHO
c                      file is now just ozone column; replaced haze
c                      dimension with terrain height in photo file
c        04/04/12      Introduced 4-dim interpolation of J values over
c                      zenith, height, albedo, terrain
c        04/12/12      Added T and P adjustments to photolysis rates
c 
c     Input arguments: 
c        iozon               ozone column index
c        alb                 surface albedo
c        terrain             terrain height (km)
c        hght                height AGL (km)
c        zenith              solar zenith angle (deg)
c        cldtrns             enery transmission coefficient
c        ldark               darkness flag (T=dark)
c        temp                temperature (K)
c        pres                pressure (mb)
c 
c     Output arguments: 
c        none
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
      integer iozon
      real alb,terrain,hght,zenith,cldtrns
      real temp,pres
      logical ldark
c
c-----Local
c
      integer nphot,irxn,ihght,izen,ialb,itrn
      integer i
      real tmp,w1,w2,w3,w4
      real prkn1111,prkn1112,prkn1121,prkn1122,prkn1211,prkn1212,
     &     prkn1221,prkn1222,prkn2111,prkn2112,prkn2121,prkn2122,
     &     prkn2211,prkn2212,prkn2221,prkn2222,tmp11,tmp12,tmp13,
     &     tmp14,tmp15,tmp16,tmp17,tmp18,tmp21,tmp22,tmp23,tmp24,
     &     tmp31,tmp32
      real tref,pref,adj1,adj2
      real no2tadj(NZEN),o3tadj(NZEN),hchotadj1(NZEN),hchotadj2(NZEN),
     &     hchopadj(NZEN),ch3chopadj(NZEN)
c
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
      if (tmp.gt.1.) then ! If height exceeds upper limit
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
      if (tmp.gt.1.) then ! If zenith exceeds upper limit
        izen = NZEN-1
        w2 = 0.
      endif
c
c-----Locate the point in albedo axis
c
      ialb = 1
      do i = 1,NALB-1
        if (alb.ge.albcl(i)) ialb = i
      enddo
      tmp = (alb - albcl(ialb))/(albcl(ialb+1) - albcl(ialb))
      w3 = 1. - tmp
c
      if (tmp.lt.0.) then ! If albedo exceeds lower limit
        w3 = 1.
      endif
      if (tmp.gt.1.) then ! If albedo exceeds upper limit
        ialb = NALB-1
        w3 = 0.
      endif
c
c-----Locate the point in terrain axis
c
      itrn = 1
      do i = 1,NTRN-1
        if (terrain.ge.trncl(i)) itrn = i
      enddo
      tmp = (terrain - trncl(itrn))/(trncl(itrn+1) - trncl(itrn))
      w4 = 1. - tmp
c
      if (tmp.lt.0.) then ! If terrain exceeds lower limit
        w4 = 1.
      endif
      if (tmp.gt.1.) then ! If terrain exceeds upper limit
        itrn = NTRN-1
        w4 = 0.
      endif
c
c-----Set the primary photolysis rates using interpolation
c     in height, zenith angle, albedo, and terrain elevation
c
      do irxn = 1,nphot1
        prkn1111 = prkn(izen,  irxn,ihght,  itrn,  ialb,  iozon)
        prkn1112 = prkn(izen,  irxn,ihght+1,itrn,  ialb,  iozon)
        prkn1121 = prkn(izen+1,irxn,ihght,  itrn,  ialb,  iozon)
        prkn1122 = prkn(izen+1,irxn,ihght+1,itrn,  ialb,  iozon)
        prkn1211 = prkn(izen,  irxn,ihght,  itrn,  ialb+1,iozon)
        prkn1212 = prkn(izen,  irxn,ihght+1,itrn,  ialb+1,iozon)
        prkn1221 = prkn(izen+1,irxn,ihght,  itrn,  ialb+1,iozon)
        prkn1222 = prkn(izen+1,irxn,ihght+1,itrn,  ialb+1,iozon)
        prkn2111 = prkn(izen,  irxn,ihght,  itrn+1,ialb,  iozon)
        prkn2112 = prkn(izen,  irxn,ihght+1,itrn+1,ialb,  iozon)
        prkn2121 = prkn(izen+1,irxn,ihght,  itrn+1,ialb,  iozon)
        prkn2122 = prkn(izen+1,irxn,ihght+1,itrn+1,ialb,  iozon)
        prkn2211 = prkn(izen,  irxn,ihght,  itrn+1,ialb+1,iozon)
        prkn2212 = prkn(izen,  irxn,ihght+1,itrn+1,ialb+1,iozon)
        prkn2221 = prkn(izen+1,irxn,ihght,  itrn+1,ialb+1,iozon)
        prkn2222 = prkn(izen+1,irxn,ihght+1,itrn+1,ialb+1,iozon)
        tmp11 = w1*prkn1111 + (1.-w1)*prkn1112
        tmp12 = w1*prkn1121 + (1.-w1)*prkn1122
        tmp13 = w1*prkn1211 + (1.-w1)*prkn1212
        tmp14 = w1*prkn1221 + (1.-w1)*prkn1222
        tmp15 = w1*prkn2111 + (1.-w1)*prkn2112
        tmp16 = w1*prkn2121 + (1.-w1)*prkn2122
        tmp17 = w1*prkn2211 + (1.-w1)*prkn2212
        tmp18 = w1*prkn2221 + (1.-w1)*prkn2222
        tmp21 = w2*tmp11 + (1.-w2)*tmp12
        tmp22 = w2*tmp13 + (1.-w2)*tmp14
        tmp23 = w2*tmp15 + (1.-w2)*tmp16
        tmp24 = w2*tmp17 + (1.-w2)*tmp18
        tmp31 = w3*tmp21 + (1.-w3)*tmp22
        tmp32 = w3*tmp23 + (1.-w3)*tmp24
        rk(idphot1(irxn)) = w4*tmp31 + (1.-w4)*tmp32
      enddo
c
c-----Cloud/aerosol adjustment
c
      do irxn = 1,nphot1
        rk(idphot1(irxn)) = rk(idphot1(irxn))*cldtrns
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
