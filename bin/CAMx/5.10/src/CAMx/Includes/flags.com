c----CAMx v5.10 090918
c 
c     FLAGS.COM contains all model flags except those related to chemistry
c                           
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c           
c     Modifications: 
c        4/26/99   Removed LSMO, LBOT flags and replaced with IADVCT flag 
c        7/05/02   Removed lpig and replaced it with ipigflg
c        4/2/03    Removed option for UAM-V type cloud adjustment
c        5/25/06   Added HDF output flag
c       10/09/08   Added ACM2 flag
c
c-----------------------------------------------------------------------
c     Integer flags:
c
c     ipigflg -- PiG flag: GREASD or IRON
c     iadvct  -- Advection solver flag
c                (2 = Bott, 3 = Piecewise Parabolic Method)
c
c     Logical flags:
c
c     lrstrt  -- simulation restart flag
c     lchem   -- chemistry flag
c     ldry    -- dry deposition flag
c     lwet    -- wet deposition flag
c     lutm    -- UTM projection flag
c     llatlon -- lat/lon coordinates flag
c     lpolar  -- polar stereographic projection flag
c     lambrt  -- Lambert conformal projection flag
c     lstagw  -- staggered input wind field flag
c     larsrc  -- area source flag
c     lptsrc  -- point source flag
c     le1day  -- 1-day emission input flag
c     lairqul -- output average file flag
c     ldiag   -- Diagnostic check flag
c     lhdfout -- HDF output file flag
c     lmpi    -- flag to determine if the model is being run with MPI
c     lsuper  -- Super stepping advection flag (not allowed in MPI)
c     lacm2   -- ACM2 flag
c
c-----------------------------------------------------------------------
c
      integer ipigflg
      integer iadvct
      logical lrstrt
      logical lchem
      logical ldry
      logical lwet
      logical lutm
      logical llatlon
      logical lpolar
      logical lambrt
      logical lstagw
      logical larsrc
      logical lptsrc
      logical le1day
      logical lairqul
      logical ldiag
      logical lhdfout
      logical lmpi
      logical lsuper
      logical lacm2
c
      common /flags/ lrstrt, lchem, ldry, lwet, lutm, llatlon, lpolar,
     &               lambrt, lstagw, larsrc, lptsrc, le1day, lairqul,
     &               ldiag, lhdfout, iadvct, ipigflg, lmpi, lsuper,
     &               lacm2
