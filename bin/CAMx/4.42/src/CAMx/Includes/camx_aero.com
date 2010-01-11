c
c-----CAMx v4.42 070603
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
      real nacl,co2,foa,mhp,paa,caco3,mgco3,a3fe,b2mn,potcl
      common /radmx/ nacl,co2,foa,mhp,paa,caco3,mgco3,a3fe,b2mn,potcl

      real eps,e0,lv,rv
      common /rhcalx/ eps,e0,lv,rv
