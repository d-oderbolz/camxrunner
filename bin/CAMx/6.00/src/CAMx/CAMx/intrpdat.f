      subroutine intrpdat
      use filunit
      use grid
      use camxcom
      use camxfld
      use o3colmap
c
c----CAMx v6.00 130506
c
c     INTRPDAT generates fine grid met data fields from parent grids for those
c     fields not read.  Also determines time-rate of change for all 
c     time-interpolated variables.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        8/30/02    Modifications for combined cloud/rain file, and to allow
c                   water vapor and cloud/rain files to be read for each nest
c        3/3/04     Added checks for interpolating water fields  
c                   if either chemistry, dry dep or wet dep is on
c       10/12/04    Water vapor and vertical diffusivity fields are now
c                   time-interpolated
c        6/21/05    Added new rain, snow, and graupel fields
c       11/04/09    Removed setting nest grid boundary winds from parent
c       01/04/11    Revised for new met input format
c       04/02/12    Removed RADM cloud adjustment option, cloud/aerosol
c                   adjustments now always done with in-line TUV;
c       11/05/12    Removed vertical nesting
c
c     Input arguments:
c        none
c
c     Output arguments:
c        none
c
c     Subroutine called:
c        INTERP2D
c        FINWIND
c        TIMRATES
c
c     Called by:
c        TSTEP_INIT
c
      implicit none
      include "camx.prm"
      include "flags.inc"
c
      integer ip,ic,igrd
c
c-----Entry point
c
      do 100 ip = 1,ngrid
        do 99 ic = 1,nchdrn(ip)
          igrd = idchdrn(ic,ip)
          if (i3dmet(igrd).eq.0) then
c
c-----Vertical grid structure
c
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Assigning heights from parent grid',
     &                             time, date,' grid',igrd
            call rassgn3d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    hnxt(iptr3d(ip)),hnxt(iptr3d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),nlay(igrd),
     &                    height(iptr3d(igrd)),hnxt(iptr3d(igrd)),
     &                    phpt(iptr3d(igrd)))
c
c-----Wind field
c
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating winds from parent grid',
     &                             time, date,' grid',igrd
            call finwind(ncol(ip),nrow(ip),nlay(ip),i1(igrd),
     &                 j1(igrd),nmesh(igrd),ncol(igrd),
     &                 nrow(igrd),nlay(igrd),unxt(iptr3d(ip)),
     &                 vnxt(iptr3d(ip)),
     &                 unxt(iptr3d(igrd)),vnxt(iptr3d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),nlay(igrd),
     &                  windu(iptr3d(igrd)),unxt(iptr3d(igrd)),
     &                  pupt(iptr3d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),nlay(igrd),
     &                  windv(iptr3d(igrd)),vnxt(iptr3d(igrd)),
     &                  pvpt(iptr3d(igrd)))
c
c-----Pressure
c
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating pressure from parent grid',
     &                             time, date,' grid',igrd
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    pnxt(iptr3d(ip)),pnxt(iptr3d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),nlay(igrd),
     &                    press(iptr3d(igrd)),pnxt(iptr3d(igrd)),
     &                    pppt(iptr3d(igrd)))
c
c-----Temperature
c
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating temps from parent grid',
     &                             time, date,' grid',igrd
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    tnxt(iptr3d(ip)),tnxt(iptr3d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),nlay(igrd),
     &                    tempk(iptr3d(igrd)),tnxt(iptr3d(igrd)),
     &                    ptpt(iptr3d(igrd)) )
c
c-----Water vapor
c
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating humidity from parent grid',
     &                                  time, date,' grid',igrd
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),
     &                    j1(igrd),nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    wnxt(iptr3d(ip)),wnxt(iptr3d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),nlay(igrd),
     &                    water(iptr3d(igrd)),wnxt(iptr3d(igrd)),
     &                    pwpt(iptr3d(igrd)) )
          endif
c
c-----Vertical diffusion coefficient
c
          if (ikv(igrd).eq.0) then
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating VDiff from parent grid',
     &                             time, date,' grid',igrd
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    knxt(iptr3d(ip)),knxt(iptr3d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),nlay(igrd),
     &                    rkv(iptr3d(igrd)),knxt(iptr3d(igrd)),
     &                    pkpt(iptr3d(igrd)))
          endif
c
c-----Surface temperature
c
          if (i2dmet(igrd).eq.0) then
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating sfc temp from parent grid',
     &                             time, date,' grid',igrd
            call interp2d(ncol(ip),nrow(ip),1,i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    tsnxt(iptr2d(ip)),tsnxt(iptr2d(igrd)) )
            call timrates(ncol(igrd),nrow(igrd),1,
     &                    tsurf(iptr2d(igrd)),tsnxt(iptr2d(igrd)),
     &                    pspt(iptr2d(igrd)) )
c
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                'Interpolating snowcover from parent grid',
     &                                  time, date,' grid',igrd
            call iassgn2d(ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    icdsno(iptr2d(ip)),icdsno(iptr2d(igrd)))
          endif
c
c-----Cloud/rain
c
          if (icld(igrd).eq.0) then
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating clouds from parent grid',
     &                             time, date,' grid',igrd
            call rassgn3d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    cod(iptr3d(ip)),cod(iptr3d(igrd)) )
c
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),
     &                    j1(igrd),nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    cwc(iptr3d(ip)),cwc(iptr3d(igrd)) )
c
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),
     &                    j1(igrd),nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    pwr(iptr3d(ip)),pwr(iptr3d(igrd)) )
c
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),
     &                    j1(igrd),nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    pws(iptr3d(ip)),pws(iptr3d(igrd)) )
c
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),
     &                    j1(igrd),nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    pwg(iptr3d(ip)),pwg(iptr3d(igrd)) )
          endif
c
  99    continue
 100  continue
c
      return
      end
