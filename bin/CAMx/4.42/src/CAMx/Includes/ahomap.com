c-----CAMx v4.42 070603
c  
c     AHOMAP.COM contains albedo/haze/ozone index maps and value classes
c                            
c     Copyright 1996-2007
c     ENVIRON International Corporation
c            
c     Modifications:  
c        none  
c
c-----------------------------------------------------------------------
c     Variables to contain the albedo/haze/ozone data:
c
c     albcl   -- albedo column lookup table
c     hazcl   -- haze column lookup table
c     ozcl    -- ozone column lookup table
c     ruflen  -- optional input roughness length table
c     nhdraho -- number of lines to start of time-varying data
c     icdalb  -- index by cell/grid for albedo
c     icdhaz  -- index by cell/grid for haze 
c     icdozn  -- index by cell/grid for ozone column
c     icdsno  -- optional index by cell/grid for snow cover
c     icdocn  -- optional index by cell/grid for ocean
c     icddrt  -- optional index by cell/grid for drought
c     icdruf  -- optional index by cell/grid for roughness
c     lrdalb  -- flag by grid to show data read for albedo
c     lrdsno  -- flag to show data read for snow cover
c     lrdocn  -- flag by grid to show data read for ocean
c     lrddrt  -- flag by grid to show data read for drought index
c     lrdruf  -- flag by grid to show data read for roughness
c
c-----------------------------------------------------------------------
c
      real      albcl(NALB)
      real      hazcl(NHAZE)
      real      ozcl(NOZN)
      real      ruflen(NRUF)
      integer   nhdraho
      integer   icdalb(MXVEC2D)
      integer   icdhaz(MXVEC2D)
      integer   icdozn(MXVEC2D)
      integer   icdsno(MXVEC2D)
      integer   icdocn(MXVEC2D)
      integer   icddrt(MXVEC2D)
      integer   icdruf(MXVEC2D)
      logical   lrdalb(MXGRID)
      logical   lrdsno
      logical   lrdocn(MXGRID)
      logical   lrddrt(MXGRID)
      logical   lrdruf(MXGRID)
c 
      common /ahomap/ albcl,hazcl,ozcl,ruflen,nhdraho,icdalb,
     &                icdhaz,icdozn,icdsno,icdocn,icddrt,icdruf,
     &                lrdalb,lrdsno,lrdocn,lrddrt,lrdruf
