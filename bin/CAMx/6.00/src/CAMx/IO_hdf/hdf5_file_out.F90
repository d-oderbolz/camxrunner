subroutine hdf5_file_out(otype,cdate,ctime)
!
!-----CAMx v4.5beta 070116
!
!     HDF5_FILE_OUT writes concentration fields to HDF5 files
!     following the conventions of the Regional Atmospheric Modeling System
!     (RAMS).  Both core model and probing tool fields can be written, 
!     for both computational and PiG sampling grids.  Individual HDF5 files
!     are written for each grid, and for each output time, and contain
!     all species carried by the model (core fields and optionally
!     probing tool fields).
!
!     Copyright 1996-2007
!     ENVIRON International Corporation
!            
!     Modifications:  
!        none
!
!     Input arguments:
!        otype               Output field type
!        cdate               Simulation date (YYJJJ)
!        ctime               Simulation time (HHMM)
!
!     Output arguments:
!        none
!
!     Routines called:
!        CAMx2RAMS_DATE
!        SHDF5_OPEN
!        SHDF5_OREC
!        SHDF5_CLOSE
!        RAMS_HEAD_FILL
!        RAMS_HEAD_WRITE
!
!     Called by:
!        CAMx
!
use camx_includes_hdf
use camxcom
use grid
use filunit
use camxfld
use chmstry
use pigsty
use tracer
use rtracchm

#ifdef USEHDF
use rams_head
use hdf5_utils
#endif

implicit none

character(len=*) :: otype
integer :: cdate
real :: ctime

integer :: hdf_check=0
#ifdef USEHDF
character(len=256) :: anamel
character(len=40) :: cgrid,ctype,dstring
logical :: exans
integer :: ib1,maxx,maxy,maxz
integer :: ngr,ngrds,npto,nxny,nv,iyear,imonth,idate,itime,ncount,iptr,ipij,ipm1
integer :: nx(100),ny(100),nz(100),i,j,k,nn
integer :: dims(3)
integer, save :: iclobber=1,ioaunt=88,npointer=0
real    :: dx(100),dy(100),xorgn(100),yorgn(100),zz(MXLAYER,100)
real, allocatable :: ofield(:)
type (head_table), allocatable, save :: aw_table(:)

hdf_check = 1

!-----Set file type label

if (otype == 'averag') ctype = 'CX'
if (otype == 'sample') ctype = 'CS'
if (otype == 'srfmod') ctype = 'SM'

!-----Setup grid definition variables according to the type of output

if (otype == 'sample') then
  ngrds = nsample
  do ngr = 1,ngrds
    nx(ngr) = ncolsmp(ngr)
    ny(ngr) = nrowsmp(ngr)
    nz(ngr) = 1
    dx(ngr) = 1000.*delx/meshsmp(ngr)
    dy(ngr) = 1000.*dely/meshsmp(ngr)
    xorgn(ngr) = 1000.*xorgsmp(ngr)
    yorgn(ngr) = 1000.*yorgsmp(ngr)
    zz(1,ngr) = 10.
  enddo
else
  ngrds = ngrid
  do ngr = 1,ngrds
    nx(ngr) = ncol(ngr)
    ny(ngr) = nrow(ngr)
    dx(ngr) = deltax(1,ngr)
    dy(ngr) = deltay(ngr)
    if (ngr == 1) then
      xorgn(ngr) = 1000.*xorg
      yorgn(ngr) = 1000.*yorg
    else
      xorgn(ngr) = 1000.*(xorg + (inst1(ngr) - 1)*delx) - deltax(1,ngr)
      yorgn(ngr) = 1000.*(yorg + (jnst1(ngr) - 1)*dely) - deltay(ngr)
    endif
    if (otype == 'srfmod') then
      nz(ngr) = 1
      zz(1,ngr) = 0.
    else
      if( l3davg(ngr) ) then
        nz(ngr) = nlay(ngr)
        do k = 1,nz(ngr)
          iptr = iptr3d(ngr) + nx(ngr)*ny(ngr)*(k-1)
          zz(k,ngr) = height(iptr)
        enddo 
      else
        nz(ngr) = 1
        zz(1,ngr) = 10.
      endif
    endif
  enddo
endif

!-----Find maximum CAMx dimensions and allocate arrays

maxx = maxval(nx(1:ngrds))
maxy = maxval(ny(1:ngrds))
maxz = maxval(nz(1:ngrds)) + 1

if (allocated(ofield)) deallocate(ofield)
allocate(ofield(maxx*maxy*maxz))
if (allocated(aw_table)) deallocate(aw_table) 
if (otype == 'srfmod') then
   allocate (aw_table(ntotsp*2*ngrds))
else
  if (ltrace) then
     allocate (aw_table((navspc+ntotsp+2)*ngrds))
  else
     allocate (aw_table((navspc+2)*ngrds))
  endif
endif

!-----Get date and time in RAMS terms and construct date string

call camx2rams_date(cdate,ctime,iyear,imonth,idate,itime)
dstring = '-'//trim(ctype)
ib1 = len_trim(dstring)
write(dstring(ib1+1:),100) '-',iyear,'-',imonth,'-',idate,'-',itime
100 format(a1,i4.4,a1,i2.2,a1,i2.2,a1,i6.6)

!-----Loop over grids; generate a grid-specific filename for this time,
!     open it, fill it, and close it.
!     Output file path/prefix comes from CAMx "hdfroot" variable:

ncount = 0
do ngr = 1,ngrds
   if (ngr > 9) then
     write(cgrid,'(a1,i2)') 'g',ngr
   else
     write(cgrid,'(a1,i1)') 'g',ngr
   endif
   anamel = trim(hdfroot)//trim(dstring)//'-'//trim(cgrid)//'.h5'
   inquire(file=trim(anamel),exist=exans)
   if (exans .and. iclobber == 0) then
      write(iout,'(//,a)') 'ERROR in HDF5_FILE_OUT:'
      write(iout,'(2A)') 'Output file already exists: ',trim(anamel)
      call camxerr()
   endif

   call shdf5_open(trim(anamel),'W',iclobber)
 
!-----Output topography and 3-D layer height fields on the computational domain

   if (l3davg(ngr) .and. otype == 'averag') then
      ncount = ncount + 1
      dims(1) = nx(ngr)
      dims(2) = ny(ngr)
      dims(3) = 1
      npto = nx(ngr)*ny(ngr)
      iptr = iptr2d(ngr)
      ofield(1:npto) = topo(iptr:iptr+npto-1)
      aw_table(ncount)%npointer = npointer
      aw_table(ncount)%ngrid = ngr
      aw_table(ncount)%string = 'TOPT'
      aw_table(ncount)%idim_type = 2
      aw_table(ncount)%nvalues = npto

      call shdf5_orec(aw_table(ncount)%idim_type,dims,aw_table(ncount)%string, &
                      rvara=ofield)

      ncount = ncount + 1
      dims(1) = nx(ngr)
      dims(2) = ny(ngr)
      dims(3) = nz(ngr) + 1
      nxny = nx(ngr)*ny(ngr)
      do j = 1,ny(ngr)
         do i = 1,nx(ngr)
            iptr = i + nx(ngr)*(j-1)
            ipij = iptr3d(ngr) + iptr - 1
            ofield(iptr) = -height(ipij)/2.
            ofield(iptr+nxny) = height(ipij)/2.
         enddo
      enddo
      do k = 2,nz(ngr)
         do j = 1,ny(ngr)
            do i = 1,nx(ngr)
               iptr = i + nx(ngr)*(j-1) + nx(ngr)*ny(ngr)*(k-1)
               ipij = iptr3d(ngr) + iptr - 1
               ipm1 = iptr3d(ngr) + (i-1) + nx(ngr)*(j-1) + &
                                    nx(ngr)*ny(ngr)*(k-2)
               ofield(iptr+nxny) = (height(ipij) + height(ipm1))/2.
            enddo
         enddo
      enddo
      aw_table(ncount)%npointer = npointer
      aw_table(ncount)%ngrid = ngr
      aw_table(ncount)%string = 'ZMID'
      aw_table(ncount)%idim_type = 3
      aw_table(ncount)%nvalues = nxny*(nz(ngr)+1)

      call shdf5_orec(aw_table(ncount)%idim_type,dims,aw_table(ncount)%string, &
                      rvara=ofield)

   endif

!-----Loop through the main variable table for core CAMx species
!     (CB4, SAPRC99, or PM)

   if (otype == 'averag' .or. otype == 'sample') then
     do nv = 1,navspc
        ncount = ncount + 1

        if (nz(ngr) == 1) then
           aw_table(ncount)%idim_type = 3
           dims(1) = nx(ngr)
           dims(2) = ny(ngr)
           dims(3) = 2
           nxny = nx(ngr)*ny(ngr)
           npto = nxny
           if (otype == 'averag') then
             iptr = iptr4d(ngr) + npto*nlay(ngr)*(nv - 1)
             ofield(1:npto) = avcnc(iptr:iptr+npto-1)
             ofield(npto+1:2*npto) = ofield(1:npto)
           else
             iptr = ipsmp(ngr) + npto*(nv - 1)
             ofield(1:npto) = smpcnc(iptr:iptr+npto-1)
             ofield(npto+1:2*npto) = ofield(1:npto)
           endif
        else
           aw_table(ncount)%idim_type = 3
           dims(1) = nx(ngr)
           dims(2) = ny(ngr)
           dims(3) = nz(ngr) + 1
           nxny = nx(ngr)*ny(ngr)
           npto = nxny*nz(ngr)
           iptr = iptr4d(ngr) + npto*(nv - 1)
!           ofield(1:npto) = avcnc(iptr:iptr+npto-1)
           ofield(nxny+1:nxny+npto) = avcnc(iptr:iptr+npto-1)
           ofield(1:nxny) = ofield(nxny+1:2*nxny)
        endif
        aw_table(ncount)%npointer = npointer
        aw_table(ncount)%ngrid = ngr
        aw_table(ncount)%string = 'CH-'//trim(spavg(nv))
        aw_table(ncount)%nvalues = npto + nxny

        call shdf5_orec(aw_table(ncount)%idim_type,dims,aw_table(ncount)%string, &
                        rvara=ofield)

     enddo

!-----Loop through the main variable table for Probing Tool species
!     (RTRAC, etc.) if passed

     if (ltrace) then
        do nv = 1,ntotsp
           ncount = ncount + 1
           dims(1) = nx(ngr)
           dims(2) = ny(ngr)
           dims(3) = 1
           npto = nx(ngr)*ny(ngr)
           if (otype == 'averag') then
              iptr = ipsa2d(ngr) + npto*(nv - 1)
              ofield(1:npto) = ptavrg(iptr:iptr+npto-1)
           else 
              iptr = iprtsmp(ngr) + npto*(nv - 1)
              ofield(1:npto) = rtsmpcnc(iptr:iptr+npto-1)
           endif
           aw_table(ncount)%nvalues = npto
           aw_table(ncount)%npointer = npointer
           aw_table(ncount)%ngrid = ngr
           aw_table(ncount)%string = 'PT-'//trim(ptname(nv))
           aw_table(ncount)%idim_type = 2
        
           call shdf5_orec(aw_table(ncount)%idim_type,dims, &
                           aw_table(ncount)%string,rvara=ofield)

        enddo
     endif

   elseif (otype == 'srfmod') then
     do nv = 1,ntotsp
        ncount = ncount + 1
        dims(1) = nx(ngr)
        dims(2) = ny(ngr)
        dims(3) = 1
        npto = nx(ngr)*ny(ngr)
        iptr = ipsa2d(ngr) + npto*(nv - 1)
        ofield(1:npto) = rtsolmas(iptr:iptr+npto-1)
        aw_table(ncount)%nvalues = npto
        aw_table(ncount)%npointer = npointer
        aw_table(ncount)%ngrid = ngr
        aw_table(ncount)%string = 'PT-S'//trim(ptname(nv))
        aw_table(ncount)%idim_type = 2

        call shdf5_orec(aw_table(ncount)%idim_type,dims, &
                        aw_table(ncount)%string,rvara=ofield)

        ncount = ncount + 1
        ofield(1:npto) = rtvegmas(iptr:iptr+npto-1)
        aw_table(ncount)%nvalues = npto
        aw_table(ncount)%npointer = npointer
        aw_table(ncount)%ngrid = ngr
        aw_table(ncount)%string = 'PT-V'//trim(ptname(nv))
        aw_table(ncount)%idim_type = 2

        call shdf5_orec(aw_table(ncount)%idim_type,dims, &
                        aw_table(ncount)%string,rvara=ofield)

     enddo
   endif

   call shdf5_close()

enddo

!-----Construct the header information and write header file
!     Output file path/prefix comes from CAMx "hdfroot" variable:

anamel = trim(hdfroot)//trim(dstring)//'-head.txt'
inquire(FILE=trim(anamel),EXIST=exans)
if (exans .and. iclobber == 0) then
   write(iout,'(//,a)') 'ERROR in HDF5_FILE_OUT:'
   write(iout,'(2A)') 'Output file already exists: ',trim(anamel)
   call camxerr()
endif
open(ioaunt,STATUS='REPLACE',FILE=trim(anamel),FORM='FORMATTED')

write(ioaunt,110) ncount
do nv = 1,ncount
   write(ioaunt,120) aw_table(nv)%string   &
                    ,aw_table(nv)%npointer  &
                    ,aw_table(nv)%idim_type  &
                    ,aw_table(nv)%ngrid  &
                    ,aw_table(nv)%nvalues
enddo

110 format(i6)
120 format(a16,1x,i12,i3,i3,1x,i9)

call rams_head_fill(ngrds,nx,ny,nz,MXLAYER,runmsg,begdate,begtim, &
                    iyear,imonth,idate,itime,llatlon,lutm,lpolar,lambrt, &
                    polelat,polelon,ylatc,xlonc,tlat1,tlat2,iuzon, &
                    dx,dy,xorgn,yorgn,zz)
call rams_head_write(ioaunt)

close(ioaunt)

#endif

if (hdf_check .ne. 1) then
  write(iout,'(//)')
  write(iout,'(a)') 'ERROR: You are asking for HDF output files, but you did not'
  write(iout,'(a)') 'compile HDF libraries into CAMx.' 
  write(iout,'(a)') 'Please re-compile wth "HDF=true" on the make command line.' 
  call camxerr()
endif

return
end
