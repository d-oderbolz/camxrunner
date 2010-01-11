program v3toasc

! This utility program is written in free-format Fortran 90.
!   It requires a Fortran 90 compiler to compile. On a DEC_Alpha
!   machine, type the following to compile:
!   
!   f90 -free -convert big_endian -o v3toasc.o v3toasc.f
!
!   On a 32 bit linux
!   machine (llc5, llc6), type the following to compile:
!   
!   pgf90 -Mfreeform -byteswapio -o v3toasc.o v3toasc.f
!
!
!   On a 64 bit linux
!   machine (lcsl5a), type the following to compile:
!   
!   pgf90 -Mfreeform -byteswapio -o v3toasc_64.o v3toasc.f
!
!   Modified  from readv3.f by J.Keller, PSI, September 2006, August 2008
!
!   Usage: v3toasc  v3filename fieldname 
 
  implicit none
  integer, dimension(50,20) :: bhi
  real, dimension(20,20) :: bhr
  character(len=80), dimension(50,20) :: bhic
  character(len=80), dimension(20,20) :: bhrc
  character(len=120) :: flnm
  character(len=9) :: fieldname
  integer :: iunit = 10
  integer :: iunitout = 11
  integer :: flag

  integer :: ndim
  real :: time, sample
  integer, dimension(4) :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 4) :: ordering
  character (len=24) :: start_date
  character (len=24) :: current_date
  character (len= 9) :: name
  character (len=25) :: units
  character (len=46) :: description

  integer :: l

  real, allocatable, dimension(:,:,:,:) :: data

  integer :: ierr, ier

  logical :: newtime = .TRUE.
  logical :: lmore

  call arguments(flnm, fieldname,lmore)
  lmore=.FALSE.

  print*, 'flnm = ', trim(flnm)
  open(iunit, file=flnm, form='unformatted', status='old', action='read')
  open(iunitout, file=fieldname, form='formatted', status='new', action='write')

  read(iunit, iostat=ierr) flag
  do while (ierr == 0)

     if (flag == 0) then
        read(iunit,iostat=ier) bhi, bhr, bhic, bhrc
        if(ier/=0) then
           write(*,'("Error reading big header")')
           call abort()
        endif
        call printout_big_header(bhi, bhr, bhic, bhrc)
        write(iunitout,'(10i8)') bhi
        write(iunitout,'(10e14.6)') bhr
     elseif (flag == 1) then

        READ (iunit,iostat=ier) ndim, start_index, end_index, time, staggering, ordering,&
             current_date, name, units, description
        if(ier/=0) then
           write(*,'("Error reading subheader")')
           call abort()
        endif

        if (lmore) then
           print*, 'ndim: ', ndim
           print*, 'start_index: ', start_index
           print*, 'end_index: ', end_index
           print*, 'time: ', time
           print*, 'staggering: #'//staggering//'#'
           print*, 'ordering: #'//ordering//'#'
           print*, 'date/time: #'//current_date//'#'
           print*, 'name: #'//name//'#'
           print*, 'units: #'//units//'#'
           print*, 'description: #'//description//'#'
        endif

        if (newtime) then
           write(*,'(/,A,2x, F15.5," Hours"/)') current_date, time/60.
           newtime = .FALSE.
        endif

        if (ndim == 1) then
           allocate(data(end_index(1)-start_index(1)+1, 1, 1, 1))
        elseif (ndim == 2) then
           allocate(data(end_index(1)-start_index(1)+1,end_index(2)-start_index(2)+1, 1, 1))
        elseif (ndim == 3) then
           allocate(data(end_index(1)-start_index(1)+1,end_index(2)-start_index(2)+1,&
              end_index(3)-start_index(3)+1, 1))
        endif

        read(iunit) data

        if (trim(name) == trim(fieldname)) then

          write(iunitout,'(a10,a)') name,  trim(units) 
          write(iunitout,'(I2,8(1x,I3))')&
             ndim, start_index(1),end_index(1), start_index(2),end_index(2),&
             start_index(3), end_index(3),start_index(4),end_index(4)
          write(iunitout,'(A,2x, F15.5)') current_date, time/60.
          write(iunitout,'(10e14.6)') data
        endif

        if (ndim == 3) then
            sample = data( end_index(1)/2,end_index(2)/2,end_index(3)/2,1 )
        else if (ndim == 2) then
            sample = data( end_index(1)/2,end_index(2)/2,1,1)
        else if (ndim == 1) then
            sample = data( end_index(1)/2,1,1,1)
        end if

        write(*,'(A8,1x,I1,4(1x,I3),1x,A,1x,A," : ", F20.8,1x,A)')&
             name, ndim, end_index(1), end_index(2), end_index(3), end_index(4),&
             staggering, ordering, sample, trim(units)

        deallocate(data)

     elseif (flag == 2) then
        newtime = .TRUE.
     else
        stop
     endif
     read(iunit, iostat=ierr) flag
  enddo

  write(*,'(/,"Hit the end of file of unit ", I3)') iunit

end program v3toasc

   subroutine printout_big_header(bhi, bhr, bhic, bhrc)

     implicit none
     integer, dimension(50,20) :: bhi
     real, dimension(20,20) :: bhr
     character(len=80), dimension(50,20) :: bhic
     character(len=80), dimension(20,20) :: bhrc
     integer :: i, j, v3j
   
     write(*,'(/)')
     v3j = bhi(1,1)
     if (bhi(1,1) == 11) v3j = v3j+5
     do j = 1, v3j
      if (j < 8 .or. j>10) then
        if (j == 1) write(*, '("TERRAIN Portion of big header:")')
        if (j == 2) write(*, '(/,"REGRID Portion of big header:")')
        if (j == 3) write(*, '(/,"RAWINS Portion of big header:")')
        if (j == 4) write(*, '(/,"SFC RAWINS Portion of big header:")')
        if (j == 5) write(*, '(/,"INTERP Portion of big header:")')
        if (j == 11) write(*, '(/,"MM5 Portion of big header:")')
        if (j == 6) write(*, '(/,"MM5 Substrate Temp File big header:")')
        if (j == 7) write(*, '(/,"MM5 Boundary File big header:")')
        if (j == 8) write(*, '(/,"Interpolated MM5 Portion of big header:")')
        write(*,'(/,"***Integers:"/)')
        do i = 1, size(bhi,1)
           if (bhi(i,j) /= -999) then
              write(*,'("BHI(",I3,",",I2,"):",I8," : ",A)')&
                   i, j, bhi(i,j),trim(bhic(i,j))
           endif
        enddo
   
        write(*,'(/,"***Floats:"/)')
        do i = 1, size(bhr,1)
           if (bhr(i,j) /= -999.) then
              write(*,'("BHR(",I3,",",I2,"):",F9.2," : ",A)')&
                   i, j, bhr(i,j),trim(bhrc(i,j))
           endif
        enddo
        write(*,'(/)')
      endif
     enddo
   end subroutine printout_big_header
   
   subroutine arguments(v3file, fieldname,lmore)
     implicit none
     character(len=*) :: v3file
     character(len=*) :: fieldname
     character(len=120) :: harg
     logical :: lmore
   
     integer :: ierr, i, numarg
     integer, external :: iargc

     numarg = iargc()
     call getarg(1, harg)   
     v3file = harg
     call getarg(2, harg)
     fieldname=trim(harg)

   end subroutine arguments
   
   subroutine help
     implicit none
     character(len=120) :: cmd
     call getarg(0, cmd)
   
     write(*,'(/,"Usage: ", A, " [-v] v3file ")') trim(cmd)
     write(*,'(8x, "-v     : Print extra info")')
     write(*,'(8x, "v3file : MM5v3 file name to read.")')
     write(*,'(8x, "-h     : print this help message and exit.",/)')
     stop
   end subroutine help

