c-----CAMx v4.42 070514
c 
c     PIGSTY.COM contains general Plume-in-Grid variables
c                           
c     Copyright 1996-2007
c     ENVIRON International Corporation
c           
c     Modifications: 
c        9/3/03        Updated with IRON PiG variables 
c        7/20/05       Moved PiG sampling grid variables from TRACER.COM
c       02/02/06       Removed GREASD-PiG specific variables, renamed
c                      PUFFPOL to PUFFMASS
c
c-----------------------------------------------------------------------
c     Variables for PiG submodel:
c
c     npig     -- number of active PiG puffs
c     nreactr  -- number of chemical reactors in each puff
c     loverlap -- puff chemistry overlap flag
c     ipigint  -- grid index possessing the minimum timestep of all grids
c                 for which new puffs will be released and all puffs will
c                 be transported
c     ingrd    -- grid ID in which puff resides
c     idpig    -- stack ID from which puff is released
c     xpigf    -- current x-location of puff front (km or deg)
c     xpigb    -- current x-location of puff back (km or deg)
c     ypigf    -- current y-location of puff front (km or deg)
c     ypigb    -- current y-location of puff back (km or deg)
c     zpig     -- current z-location of puff (meters)
c     axisy    -- width of puff (meters)
c     axisz    -- depth of puff (meters)
c     sigy     -- Gaussian standard deviation width (meters)
c     sigz     -- Gaussian standard deviation depth (meters)
c     lnewt    -- flag indicating newly released puff for transport
c     lnewg    -- flag indicating newly released puff for growth
c     fmspig   -- puff volume parameter
c     agepigf  -- puff age of front end (seconds)
c     agepigb  -- puff age of back end (seconds)
c     npigon   -- number of active puffs in grid
c     pufftop  -- height of top of puff (m)
c     puffbot  -- height of bottom of puff (m)
c     puffmass -- puff pollutant mass
c     puffrad  -- initial guess for radicals in puff reactors
c     htfms    -- horizontal turbulent flux moment, shear (m2/s)
c     htfmb    -- horizontal turbulent flux moment, buoyancy (m2/s)
c     vtfms    -- vertical turbulent flux moment, shear (m2/s)
c     vtfmb    -- vertical turbulent flux moment, buoyancy (m2/s)
c     nkill    -- number of puffs killed by the following processes:
c                 1 = size
c                 2 = fraction of remaining mass (or age)
c                 3 = convergence error in LSODE (background step)
c                 4 = convergence error in LSODE (total puff step)
c                 5 = puff entered Stage 3 chem
c                 6 = Neg concs in aqueous PM chem (background step)
c                 7 = Neg concs in aqueous PM chem (total puff step)
c                 8 = Neg concs in PM chem (background step)
c                 9 = Neg concs in PM chem (total puff step)
c
c-----------------------------------------------------------------------
c
      logical loverlap,lnewt(MXPIG),lnewg(MXPIG)
      integer npig,nreactr,ipigint
      integer ingrd(MXPIG)
      integer idpig(MXPIG)
      integer nkill(9)
      real    xpigf(MXPIG),xpigb(MXPIG),ypigf(MXPIG),ypigb(MXPIG)
      real    zpig(MXPIG)
      real    axisy(MXPIG),axisz(MXPIG)
      real    sigy(MXPIG),sigz(MXPIG)
      real    fmspig(MXPIG)
      real    agepigf(MXPIG),agepigb(MXPIG)
      integer npigon(MXGRID)
      real    pufftop(MXPIG),puffbot(MXPIG)
      real    puffrad(MXRADCL,MXRECTR,MXPIG)
      real    puffmass(MXSPEC,MXRECTR,MXPIG)
      real    htfms(MXPIG),htfmb(MXPIG),vtfms(MXPIG),vtfmb(MXPIG)
c
      common /pig1/ npig,nreactr,loverlap,ipigint,ingrd,idpig,nkill,
     &              xpigf,xpigb,ypigf,ypigb,zpig,axisy,axisz,sigy,sigz,
     &              lnewt,lnewg,fmspig,agepigf,agepigb,npigon,pufftop,
     &              puffbot,puffrad,puffmass,htfms,htfmb,vtfms,vtfmb
c
c-----------------------------------------------------------------------
c  Parameters for PiG sampling grids:
c-----------------------------------------------------------------------
c
c   lsample  L  sampling grid flag
c   lbckgrd  L  include background concs in sampling grid output
c   nsample  I  number of sampling grids
c   ismp1,2  I  x-dir beginning/ending master grid cells
c   jsmp1,2  I  y-dir beginning/ending master grid cells
c   meshsmp  I  sampling grid meshing factor relative to master grid
c   ncols    I  number of sampling grid columns
c   nrows    I  number of sampling grid rows
c   ismpgrd  I  pointer to finest nest containing sampling grid
c   ipsmp    I  pointer array for 2-D PiG sampling grid variables
c   xorgs    R  x-origin of sampling grid (deg or km)
c   yorgs    R  y-origin of sampling grid (deg or km)
c   smpcnc   R  time-averaged concentration array on sampling grid
c               (gasses=ppm, PM=ug/m3)
c
      logical lsample,lbckgrd
      integer nsample,ismp1(MXSAMPLE),ismp2(MXSAMPLE),
     &        jsmp1(MXSAMPLE),jsmp2(MXSAMPLE),meshsmp(MXSAMPLE),
     &        ncols(MXSAMPLE),nrows(MXSAMPLE),ismpgrd(MXSAMPLE),
     &        ipsmp(MXSAMPLE)
      real xorgs(MXSAMPLE),yorgs(MXSAMPLE),smpcnc(MXSMP2D)
c
      common /pigsamp/ lsample,lbckgrd,nsample,ismp1,ismp2,
     &                 jsmp1,jsmp2,meshsmp,ncols,nrows,ismpgrd,
     &                 ipsmp,xorgs,yorgs,smpcnc
