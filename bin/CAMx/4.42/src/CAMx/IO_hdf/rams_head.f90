module rams_head

type rheader
   integer :: ngrids,nzg,nzs,npatch,iyear1,imonth1,idate1,itime1  &
             ,ihtran,if_adap,utmzn
   integer, pointer, dimension(:) :: nnxp,nnyp,nnzp
   character(len=128) :: expnme
   real :: time,centlat,centlon,stdlat1,stdlat2
   real, pointer, dimension(:) :: deltaxn,deltayn,platn,plonn,slz
   real, pointer, dimension(:,:) :: xtn,xmn,ytn,ymn,ztn,zmn
end type
type(rheader) :: r

type head_table
   character(len=16) :: string
   integer :: npointer,idim_type,ngrid,nvalues
end type
type (head_table), allocatable :: anal_table(:)

contains

!--------------------------------------------------------------------------

subroutine rams_head_fill(ngrid,ncol,nrow,nlay,mxlaya,runmsg,begdate,begtim, &
                          iyear,imonth,idate,itime,latlon,lutm,lpolar,lambrt, &
                          polelat,polelon,clat,clon,tlat1,tlat2,utmzone, &
                          deltax,deltay,xorg,yorg,height)

implicit none
integer :: ngrid,ncol(ngrid),nrow(ngrid),nlay(ngrid),mxlaya,begdate, &
           iyear,imonth,idate,itime,utmzone
real    :: begtim,polelat,polelon,clat,clon,tlat1,tlat2,deltax(ngrid), &
           deltay(ngrid),height(mxlaya,ngrid),xorg(ngrid),yorg(ngrid)
character(len=80) :: runmsg
logical :: latlon,lutm,lpolar,lambrt

integer :: maxx,maxy,maxz,ng,i,j
real :: swx,swy
character(len=14) :: ctime1,ctime2

!-----Find maximum CAMx dimensions

maxx = maxval(ncol(1:ngrid))
maxy = maxval(nrow(1:ngrid))
maxz = maxval(nlay(1:ngrid)) + 1
   
!-----Allocate arrays in header

allocate(r%slz(1))
allocate(r%nnxp(ngrid),r%nnyp(ngrid),r%nnzp(ngrid))
allocate(r%deltaxn(ngrid),r%deltayn(ngrid),r%platn(ngrid),r%plonn(ngrid))
allocate(r%xtn(maxx,ngrid),r%xmn(maxx,ngrid))
allocate(r%ytn(maxy,ngrid),r%ymn(maxy,ngrid))
allocate(r%ztn(maxz,ngrid),r%zmn(maxz,ngrid))
allocate(r%slz(1))

!-----Start fillin'

r%expnme = runmsg
r%ngrids = ngrid
r%nnxp(1:ngrid) = ncol(1:ngrid)
r%nnyp(1:ngrid) = nrow(1:ngrid)
r%nnzp(1:ngrid) = nlay(1:ngrid) + 1
r%deltaxn(1:ngrid) = deltax(1:ngrid)
r%deltayn(1:ngrid) = deltay(1:ngrid)

if (lpolar) then
   r%ihtran = 1
   r%platn(1:ngrid) = polelat
   r%plonn(1:ngrid) = polelon
elseif (lambrt) then
   r%ihtran = 2
   r%platn(1:ngrid) = clat
   r%plonn(1:ngrid) = clon
   r%centlat = clat
   r%centlon = clon
   r%stdlat1 = amin1(abs(tlat1),abs(tlat2))
   r%stdlat2 = amax1(abs(tlat1),abs(tlat2))
elseif (lutm) then
   r%ihtran = 0
   r%utmzn = utmzone
elseif (latlon) then
   r%ihtran = 3
endif

!-----Soil/snow stuff are dummies

r%npatch = 1
r%nzg = 1
r%nzs = 1
r%slz(r%nzg) = 0.
r%if_adap = 0

!-----Get beginning date and time in RAMS terms

call camx2rams_date(begdate,begtim,r%iyear1,r%imonth1,r%idate1,r%itime1)
call date_make_big(r%iyear1,r%imonth1,r%idate1,r%itime1,ctime1)
call date_make_big(iyear,imonth,idate,itime,ctime2)
call date_subtract(ctime1,ctime2,r%time,'s')
r%itime1 = r%itime1/100

!-----X/Y gridpoint coordinates for each grid

do ng = 1,r%ngrids
   swx = xorg(ng)
   do i = 1,r%nnxp(ng)
     r%xtn(i,ng) = swx + (i - 0.5)*r%deltaxn(ng)
     r%xmn(i,ng) = swx + (i      )*r%deltaxn(ng)
   enddo
   swy = yorg(ng)
   do j = 1,r%nnyp(ng)
     r%ytn(j,ng) = swy + (j - 0.5)*r%deltayn(ng)
     r%ymn(j,ng) = swy + (j      )*r%deltayn(ng)
   enddo
   call get_rams_heights(height(1,ng),r%nnzp(ng),r%zmn(1,ng),r%ztn(1,ng))
enddo

return
end subroutine

!--------------------------------------------------------------------------

subroutine get_rams_heights(zc,nz,zm,zt)

implicit none
integer :: nz
real :: zc(nz-1),zt(nz),zm(nz)

integer :: k

zm(1) = 0.
do k = 2,nz
   zm(k) = zc(k-1)
enddo

zt(1) = -(zm(2) + zm(1))*0.5
do k = 2,nz
   zt(k) = (zm(k) + zm(k-1))*0.5
enddo

return
end subroutine

!--------------------------------------------------------------------------

subroutine rams_head_write(iun)

implicit none
integer :: iun

integer :: ie,ng,idums,iduma(1)
real :: fdums,fduma(1)
character(len=2) :: cng
character(len=128) :: cdums,cduma(1)

ie = cio_c(iun,'expnme',r%expnme,cduma,1)
ie = cio_f(iun,'time',r%time,fduma,1)
ie = cio_i(iun,'ngrids',r%ngrids,iduma,1)
ie = cio_i(iun,'nnxp',r%nnxp(1),r%nnxp,r%ngrids)
ie = cio_i(iun,'nnyp',r%nnyp(1),r%nnyp,r%ngrids)
ie = cio_i(iun,'nnzp',r%nnzp(1),r%nnzp,r%ngrids)
ie = cio_i(iun,'nzg',r%nzg,iduma,1)
ie = cio_i(iun,'nzs',r%nzs,iduma,1)
ie = cio_i(iun,'npatch',r%npatch,iduma,1)
ie = cio_i(iun,'if_adap',r%if_adap,iduma,1)
ie = cio_i(iun,'ihtran',r%ihtran,iduma,1)
ie = cio_f(iun,'deltaxn',r%deltaxn(1),r%deltaxn,r%ngrids)
ie = cio_f(iun,'deltayn',r%deltayn(1),r%deltayn,r%ngrids)
ie = cio_f(iun,'platn',r%platn(1),r%platn,r%ngrids)
ie = cio_f(iun,'plonn',r%plonn(1),r%plonn,r%ngrids)
ie = cio_f(iun,'centlat',r%centlat,fduma,1)
ie = cio_f(iun,'centlon',r%centlon,fduma,1)
ie = cio_f(iun,'stdlat1',r%stdlat1,fduma,1)
ie = cio_f(iun,'stdlat2',r%stdlat2,fduma,1)
ie = cio_i(iun,'utmzone',r%utmzn,iduma,1)
ie = cio_i(iun,'iyear1',r%iyear1,iduma,1)
ie = cio_i(iun,'imonth1',r%imonth1,iduma,1)
ie = cio_i(iun,'idate1',r%idate1,iduma,1)
ie = cio_i(iun,'itime1',r%itime1,iduma,1)

do ng = 1,r%ngrids
   write(cng,'(i2.2)') ng
   ie = cio_f(iun,'xmn'//cng,fdums,r%xmn(1,ng),r%nnxp(ng))
   ie = cio_f(iun,'xtn'//cng,fdums,r%xtn(1,ng),r%nnxp(ng))
   ie = cio_f(iun,'ymn'//cng,fdums,r%ymn(1,ng),r%nnyp(ng))
   ie = cio_f(iun,'ytn'//cng,fdums,r%ytn(1,ng),r%nnyp(ng))
   ie = cio_f(iun,'zmn'//cng,fdums,r%zmn(1,ng),r%nnzp(ng))
   ie = cio_f(iun,'ztn'//cng,fdums,r%ztn(1,ng),r%nnzp(ng))
   ie = cio_f(iun,'slz'//cng,r%slz(1),fduma,r%nzg)
enddo

return
end subroutine

!--------------------------------------------------------------------------

subroutine dayjul(juldate,imonth,iday,iyear)

implicit none
integer :: juldate,imonth,iday,iyear

integer, save :: mondays(12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
integer :: jday,im,mm

!-----Compute a month and day given a julian day and year

if (juldate < 1 .or. juldate > 366) then
   print*,'dayjul: illegal julian day:',juldate
   stop 'dayjul'
endif

jday = juldate
do im = 1,12
   mm = mondays(im)
   if (im == 2) mm = mm + (1 - min(1,mod(iyear,4)))
   if (jday <= mm) then
      imonth = im
      iday = jday
      return
   endif
   jday = jday - mm
enddo

return
end subroutine

!--------------------------------------------------------------------------

subroutine camx2rams_date(cdate,ctime,iyear,imonth,idate,itime)

implicit none
integer :: cdate,iyear,imonth,idate,itime
real :: ctime

integer :: rhrs,rmins,rsecs,jday,cctime

!-----Convert CAMx date/time to RAMS date/time

iyear = cdate/1000
jday = mod(cdate,1000)
if (iyear < 50) then
   iyear = iyear + 2000
else
   iyear = iyear + 1900
endif
call dayjul(jday,imonth,idate,iyear)

!-----ctime is camx variable in hhmm. (fraction of min)

cctime = nint(ctime*1000)
rhrs = cctime/100000
rmins = mod(cctime/1000,100)
rsecs = mod(cctime,1000)*60/1000
itime = rhrs*10000+rmins*100+rsecs

return
end subroutine

!--------------------------------------------------------------------------

subroutine date_abs_secs(indate1,seconds)
implicit none
character(len=14) :: indate1
real(kind=8) :: seconds

! compute number of seconds past 1 January 1900 12:00 am

real(kind=8) :: s1,s2,s3,s4
integer :: year1,month1,date1,hour1,iy,ndays

call date_unmake_big(year1,month1,date1,hour1,indate1)

iy = year1 - 1900
ndays = iy*365 + (iy-1)/4 + julday(month1,date1,iy)
s1 = dble(ndays) *86400.
s2 = dble(hour1/10000)*3600.
s3 = dble(mod(hour1,10000)/100)*60.
s4 = dble(mod(hour1,100))
seconds = s1+s2+s3+s4

return
end subroutine

!--------------------------------------------------------------------------

subroutine date_subtract(indate1,indate2,tinc,tunits)
implicit none
real :: tinc
character(len=1) :: tunits
character(len=14) :: indate1, indate2

! add (or subracts) a time increment to a date and output new date
! -> uses hhmmss for hours, 4 digit year

integer :: mondays(12)
data mondays/31,28,31,30,31,30,31,31,30,31,30,31/
integer :: year1,month1,date1,hour1,year2,month2,date2,hour2
real(kind=8) :: secs1,secs2
real :: ttinc

call date_abs_secs(indate1,secs1)
call date_abs_secs(indate2,secs2)

! convert time to requested unit

ttinc = secs2 - secs1
if (tunits.eq.'s') tinc = ttinc
if (tunits.eq.'m') tinc = ttinc/60.
if (tunits.eq.'h') tinc = ttinc/3600.
if (tunits.eq.'d') tinc = ttinc/86400.

return
end subroutine

!--------------------------------------------------------------------------

subroutine date_make_big (inyear,inmonth,indate,inhour,outdate)
implicit none
integer :: inyear,inmonth,indate,inhour
character(len=14) ::  outdate

write(outdate(1:4),10) inyear
write(outdate(5:6),11) inmonth
write(outdate(7:8),11) indate
write(outdate(9:14),12) inhour
10 format(i4.4)
11 format(i2.2)
12 format(i6.6)

return
end subroutine

!--------------------------------------------------------------------------

subroutine date_unmake_big (inyear,inmonth,indate,inhour,outdate)
implicit none
integer :: inyear,inmonth,indate,inhour
character(len=14) :: outdate

read(outdate(1:4),10) inyear
read(outdate(5:6),11) inmonth
read(outdate(7:8),11) indate
read(outdate(9:14),12) inhour
10 format(i4)
11 format(i2)
12 format(i6)

return
end subroutine

!--------------------------------------------------------------------------

integer function julday(imonth,iday,iyear)
implicit none
integer :: imonth,iday,iyear

! compute the julian day from a normal date

julday = iday  &
       + min(1,max(0,imonth-1))*31  &
       + min(1,max(0,imonth-2))*(28+(1-min(1,mod(iyear,4))))  &
       + min(1,max(0,imonth-3))*31  &
       + min(1,max(0,imonth-4))*30  &
       + min(1,max(0,imonth-5))*31  &
       + min(1,max(0,imonth-6))*30  &
       + min(1,max(0,imonth-7))*31  &
       + min(1,max(0,imonth-8))*31  &
       + min(1,max(0,imonth-9))*30  &
       + min(1,max(0,imonth-10))*31  &
       + min(1,max(0,imonth-11))*30  &
       + min(1,max(0,imonth-12))*31

return
end function

!--------------------------------------------------------------------------

integer function cio_i(iun,cstr,is,ia,n)
implicit none
integer :: iun,n,i
integer ia(*),is
character(len=*) :: cstr

if (n == 1) ia(1) = is
write(iun,20) cstr
write(iun,*) n
write(iun,10) (ia(i),i=1,n)
10 format(i6)
20 format('__',a)
cio_i = 0

return
end function

!--------------------------------------------------------------------------

integer function cio_f(iun,cstr,is,ia,n)
implicit none
integer :: iun,n,i
real ia(*),is
character(len=*) :: cstr

if (n == 1) ia(1) = is
write(iun,20) cstr
write(iun,*) n
write(iun,10) (ia(i),i=1,n)
10 format(e16.8)
20 format('__',a)
cio_f = 0

return
end function

!--------------------------------------------------------------------------

integer function cio_c(iun,cstr,is,ia,n)
implicit none
integer :: iun,n,i
character(len=*) :: ia(*),is
character(len=*) :: cstr

if (n == 1) ia(1) = is
write(iun,20) cstr
write(iun,*) n
write(iun,10) (ia(i),i=1,n)
10 format(a)
20 format('__',a)
cio_c = 0

return
end function

end module
