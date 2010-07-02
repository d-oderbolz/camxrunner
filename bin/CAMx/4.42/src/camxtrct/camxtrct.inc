C*** CAMXTRCT.INC
c
c-----------------------------------------------------------------------
c   Include file for parameters and common variables for the CAMXTRCT
c   program
c
c-----------------------------------------------------------------------
c  Parameters for Version number:
c-----------------------------------------------------------------------
c
c   VERSON C  character string for version number
c
      character*35 VERSON
c
      parameter( VERSON = 'CAMxtract: Average file extraction' )
c
c-----------------------------------------------------------------------
c  Parameters for I/O units:
c-----------------------------------------------------------------------
c
c   IORSTD   I  unit number for standard input
c   IOWSTD   I  unit number for standard output
c
c   IOWGRD   I  unit number for output file
c   IORFAV   I  unit number for the Find grid AVERAGE file 
c   IORCAV   I  unit number for the Coarse grid AVERAGE file 
c   IORBND   I  unit number for the BOUNDARY file 
c   IORSTN   I  unit number for the station file
c   IORTMP   I  unit number for the temperature file
c   IORZP    I  unit number for the coarse grid height/pressure file
c   IORZP+i  I  unit number for the nested grid height/pressure files
c 
      integer*4 IORSTD
      integer*4 IOWSTD
c
      integer*4 IOWGRD
      integer*4 IORFAV
      integer*4 IORCAV
      integer*4 IORBND
      integer*4 IORSTN
      integer*4 IORTMP
      integer*4 IORZP
c
      parameter( IORSTD =  5 )
      parameter( IOWSTD =  6 )
c
      parameter( IOWGRD = 30 )
      parameter( IORFAV = 10 )
      parameter( IORCAV = 11 )
      parameter( IORBND = 12 )
      parameter( IORSTN = 13 )
      parameter( IORTMP = 14 )
      parameter( IORZP  = 20 )
c
c-----------------------------------------------------------------------
c  Parameters for array dimensions:
c-----------------------------------------------------------------------
c
c   MXSPEC I   maximum number of species
c   MXCELL I   maximum number of cells
c   MXLAYR I   maximum number of layers
c   MXSTN  I   maximum number of stations
c   MXGRID I   maximum number of grids (for nests)
c
      integer*4 MXSPEC
      integer*4 MXCELL
      integer*4 MXLAYR
      integer*4 MXSTN
      integer*4 MXGRID
c
      parameter( MXSPEC =  40 )
      parameter( MXCELL = 200 )
      parameter( MXLAYR =  10 )
      parameter( MXSTN  =   1 )
      parameter( MXGRID =   2 )
c
c-----------------------------------------------------------------------
c  Parameters for error codes:
c-----------------------------------------------------------------------
c
c   ISUCES  I  error code for successful execution
c   IFAIL   I  error code for error encountered
c   IEOF    I  error code for END-OF-FILE reached
c
      integer*4 ISUCES
      integer*4 IFAIL
      integer*4 IEOF
c
      parameter( ISUCES =  1 )
      parameter( IFAIL  = -1 )
      parameter( IEOF   =  2 )
c
c-----------------------------------------------------------------------
c  Parameters for type of extraction:
c-----------------------------------------------------------------------
c
c   XYHOUR   C   code for "XY HOURS" extraction
c   XZHOUR   C   code for "XZ HOURS" extraction
c   YZHOUR   C   code for "YZ HOURS" extraction
c   XYMAX    C   code for "XY MAX" extraction
c   XZMAX    C   code for "XZ MAX" extraction
c   YZMAX    C   code for "YZ MAX" extraction
c   TIMSER   C   code for "TIME SERIES" extraction
c   EMISS    C   code for "EMISSIONS" extraction
c   PTSRCE   C   code for "PTSOURCE" extraction
c
      character*15 XYHOUR
      character*15 XZHOUR
      character*15 YZHOUR
      character*15 XYMAX
      character*15 XZMAX
      character*15 YZMAX
      character*15 TIMSER
      character*15 EMISS
      character*15 PTSRCE
c
      parameter( XYHOUR = 'XY HOURS       ' )
      parameter( XZHOUR = 'XZ HOURS       ' )
      parameter( YZHOUR = 'YZ HOURS       ' )
      parameter( XYMAX  = 'XY MAX         ' )
      parameter( XZMAX  = 'XZ MAX         ' )
      parameter( YZMAX  = 'YZ MAX         ' )
      parameter( TIMSER = 'TIME SERIES    ' )
      parameter( EMISS  = 'EMISSIONS      ' )
      parameter( PTSRCE = 'PTSOURCE       ' )
c
c-----------------------------------------------------------------------
c  Parameters for type of average file:
c-----------------------------------------------------------------------
c
c   COARSE  C  code for COARSE grid file
c   FINE    C  code for FINE grid file
c   ALL     C  code for COARSE/FINE grid extraction
c 
      character*10 COARSE
      character*10 FINE
      character*10 ALL
c
      parameter( COARSE = 'COARSE' )
      parameter( FINE   = 'FINE'   )
      parameter( ALL    = 'ALL'   )
c
c-----------------------------------------------------------------------
c  Parameters for type of output file:
c-----------------------------------------------------------------------
c
c   ASCII   C  code for ASCII type of output file (Surfer GRD format)
c   BINARY  C  code for BINARY type of output file (CAMx Average format)
c
      character*10 ASCII
      character*10 BINARY
c
      parameter( ASCII  = 'ASCII' )
      parameter( BINARY = 'BINARY' )
c
c-----------------------------------------------------------------------
c  Parameters for type of layer to extract:
c-----------------------------------------------------------------------
c
c   SURFAC  C  code for SURFACE type of extraction
c   ALOFT   C  code for ALOFT type of extraction
c
      character*10 SURFAC
      character*10 ALOFT
c
      parameter( SURFAC  = 'SURFACE' )
      parameter( ALOFT = 'ALOFT' )
c
c-----------------------------------------------------------------------
c  Variables for user inputs:
c-----------------------------------------------------------------------
c
c   spname   C   species name to extract
c   exttyp   C   code for extract type 
c   avgtyp   C   code for type of average file
c   outtyp   C   code for type of output file format
c   laytyp   C   code for type of layer extraction
c   untstr   C   user supplied string for units of output
c   untfac   R   conversion factor from input to output units
c   ndec     I   number of decimal places on output 
c   epslon   R   minimum value for output
c   inter    I   time interval (in hours)
c   istart   I   first hour
c   grdnum   I   nested grid number to extract
c   irow     I   row to extract (YZ extraction) 
c   jcol     I   column to extract (XZ extraction)
c   klayer   I   layer to extract (XY extraction)
c   lbndry   L   flag to determine if boundary file should be used
c   nests    I   number of nested grids
c   nzpfil   I   number of ZP files to read in
c
      character*20 spname
      character*15 exttyp
      character*10 avgtyp
      character*10 outtyp
      character*10 laytyp
      character*10 untstr
      integer*4    inter
      integer*4    istart
      integer*4    grdnum
      integer*4    irow
      integer*4    jcol
      integer*4    klayer
      integer*4    ndec
      integer*4    nests
      integer*4    nzpfil
      real*4       epslon
      real*4       untfac
      logical*4    lbndry
c
      common /usrchr/ spname, exttyp, avgtyp, outtyp, untstr, laytyp
      common /usrdat/ untfac, ndec, inter, istart, irow, jcol, 
     &                klayer, lbndry, epslon, grdnum, nests, nzpfil
c
c-----------------------------------------------------------------------
c  Variables for domain specifications of Coarse grid:
c-----------------------------------------------------------------------
c
c   cnote   R  note on the average file
c   nxcell  I  number of cells in X direction (0 is for coarse grid)
c   nycell  I  number of cells in Y direction (0 is for coarse grid)
c   nlayer  I  number of layers  (0 is for coarse grid)
c   xorig   R  X coordinate of domain origin
c   yorig   R  Y coordinate of domain origin
c   iutmzn  I  UTM zone 
c   deltax  R  cell width in X direction
c   deltay  R  cell width in Y direction
c   iwest   I  left boundary cell location for each row in XY plane
c   ieast   I  right boundary cell location for each row in XY plane
c   inorth  I  top boundary cell location for each col in XY plane
c   isouth  I  bottom boundary cell location for each col in XY plane
c   idtbeg  I  beginning date of simulation
c   timbeg  R  beginning time of simulation
c   idtend  I  ending date of simulation
c   timend  R  ending time of simulation
c
      character*60 cnote
      integer*4    nxcell
      integer*4    nycell
      integer*4    nlayer
      integer*4    iutmzn
      integer*4    iwest(MXCELL,0:MXGRID)
      integer*4    ieast(MXCELL,0:MXGRID)
      integer*4    inorth(MXCELL,0:MXGRID)
      integer*4    isouth(MXCELL,0:MXGRID)
      integer*4    idtbeg
      integer*4    idtend
      real*4       xorig
      real*4       yorig
      real*4       deltax
      real*4       deltay
      real*4       timbeg
      real*4       timend
c
      common /domchr/ cnote
      common /domdat/ nxcell, nycell, nlayer, iutmzn, xorig, yorig, 
     &                deltax, deltay, iwest, ieast, inorth, isouth,
     &                idtbeg, idtend, timbeg, timend
c
c-----------------------------------------------------------------------
c  Variables for domain specifications of fine grids:
c-----------------------------------------------------------------------
c
c   ngrid   I  number of grids in fine grid file
c   nmesh   I  relative meshing factor for Fine grids (calculated)
c   mshfac  I  absolute meshing factor for Fine grids (read in)
c   iclbeg  I  beginning X cell of parent grid where fine grid nests
c   jclbeg  I  beginning Y cell of parent grid where fine grid nests
c   iparnt  I  grid number of parent for each fine grid
c   nxgrid  I  number of cells in X fine grid (0 is for coarse grid)
c   nygrid  I  number of cells in fine grid (0 is for coarse grid)
c   nlgrid  I  number of layers in fine grid (0 is for coarse grid)
c   grxorg  R  origin of the grid in X direction
c   gryorg  R  origin of the grid in Y direction
c   grdelx  R  cell width in X direction (km or lat)
c   grdely  R  cell width in Y direction (km or lat)
c
      integer*4 ngrid
      integer*4 nmesh(0:MXGRID),mshfac(0:MXGRID)
      integer*4 iclbeg(0:MXGRID)
      integer*4 jclbeg(0:MXGRID)
      integer*4 iparnt(0:MXGRID)
      integer*4 nxgrid(0:MXGRID)
      integer*4 nygrid(0:MXGRID)
      integer*4 nlgrid(0:MXGRID)
      real*4    grxorg(0:MXGRID)
      real*4    gryorg(0:MXGRID)
      real*4    grdelx(0:MXGRID)
      real*4    grdely(0:MXGRID)
c
      common /findat/ ngrid, nmesh, mshfac, iclbeg, jclbeg, iparnt,
     &                nxgrid, nygrid, nlgrid, grxorg, gryorg,
     &                grdelx, grdely
c
c-----------------------------------------------------------------------
c  Variables for species names:
c-----------------------------------------------------------------------
c
c    spclst  C  array of species names
c    nspec   I  number of species
c    lnumer  L  flag to determine if species contributes to numerator
c    facnum  R  factor applied to species in numerator
c    ldenom  L  flag to determine if species contributes to denominator
c    lratio  L  flag to determine if a ratio was requested
c    facden  R  factor applied to species in denominator
c
      character*10 spclst(MXSPEC)
      integer*4    nspec
      logical*4    lnumer(MXSPEC)
      real*4       facnum(MXSPEC)
      logical*4    ldenom(MXSPEC)
      logical*4    lratio
      real*4       facden(MXSPEC)
c
      common /spcchr/ spclst
      common /spcdat/ nspec, lnumer, facnum, ldenom, facden, lratio
c
c-----------------------------------------------------------------------
c  Variables for height/pressure and temperature data:
c-----------------------------------------------------------------------
c  
c     nmshv   I  meshing factor for each coarse grid layer
c     height  R  layer height array
c     depth   R  layer depth array
c     press   R  pressure array
c     temp    R  temperatures array
c
      integer*4 nmshv(MXLAYR,0:MXGRID)
      real*4  height(MXCELL,MXCELL,MXLAYR,0:MXGRID)
      real*4  press(MXCELL,MXCELL,MXLAYR,0:MXGRID)
      real*4  depth(MXCELL,MXCELL,MXLAYR,0:MXGRID)
      real*4  temp(MXCELL,MXCELL,MXLAYR,0:MXGRID)
c
      common /zpdat/ nmshv, height, press, depth, temp
c    
c-----------------------------------------------------------------------
c  Variables for station data:
c-----------------------------------------------------------------------
c
c    
