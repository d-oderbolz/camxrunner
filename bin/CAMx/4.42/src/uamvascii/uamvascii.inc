c     uamvascii.inc
      integer*4 InNum, OutNum, ErrNum

      parameter ( InNum=10 )
      parameter ( OutNum=11 )
      parameter ( ErrNum=12 )

      parameter (mxx = 300)
      parameter (mxy = 300)

      integer*4 nox, noy, nlayer, nospec, nfx, nfy, nfz, nfspec
      common /limits/ nox, noy, nlayer, nospec, nfx, nfy, nfz, nfspec

      real*4 emob(mxx,mxy)
      common /lcm/ emob

c     fland will be used for both land use types and for 
c     concentrations.  It is dimensioned large enough to handle
c     max no. of species.  No. of land use types is 11 and is set
c     explicitly in isurfcon.

      integer*4 mxspc
      parameter ( mxspc = 150 )     
      real*4 fland(mxx,mxy,mxspc)
      common /flandcom/ fland
      
      character*8  luheader
      character*80 msg
      character*10 mavspc(mxspc)
      parameter (maxfin = 10)
      common /fgpars/ msg, mavspc, mspec(10,mxspc),
     $                ixfb(maxfin), jyfb(maxfin),
     $                ixfe(maxfin), jyfe(maxfin), nhf(maxfin),
     $                nvf(maxfin), nxf(maxfin), nyf(maxfin), 
     $                nzf(maxfin), ifgptr(maxfin), ifglvl(maxfin)
      character*4 mspec
