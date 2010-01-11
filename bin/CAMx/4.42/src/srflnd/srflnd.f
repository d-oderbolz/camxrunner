      program srflnd
c
c  SRFLND -  A simple QA program check the binary CAMx landuse file
c
c  Modified by daniel.oderbolz@psi.ch to return a value to the shell.
c  0 means that the data seems to be OK, 1 means that at least one cell 
c  contains landuse fractions which in sum differ more that 1% from 1.
c Refer to the discussion in comp.lang.fortran about 
c "Fortran Main Program Return Value" for the portability of the solution
c SRFLND does not write any files anymore
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 1998  ENVIRON
c 
c This program is free software; you can redistribute it and/or
c modify it under the terms of the GNU General Public License
c as published by the Free Software Foundation; either version 2
c of the License, or (at your option) any later version.
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c
c To obtain a copy of the GNU General Public License
c write to the Free Software Foundation, Inc., 
c 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      parameter (mxx=300)
      parameter (mxy=300)
      integer lumax(mxx,mxy)
      integer length
      real fland(mxx,mxy,11), totlu(mxx,mxy), topo(mxx,mxy)
      character*250 infile

      logical sum_off

      data sum_off /.false./
      
      read(*,*) nx,ny,delta,xorg,yorg
      write(*,*) 'Enter input landuse file: '
      read (*,'(a)') infile
      open(7,file=infile,form='unformatted')

      read(7) (((fland(i,j,l),i=1,nx),j=1,ny),l=1,11)
      read(7,end=100,err=100) ((topo(i,j),i=1,nx),j=1,ny)
      goto 101
 100  do j = 1,ny
        do i = 1,nx
          topo(i,j) = 0.
        enddo
      enddo
c
c-----Write out distribution of dominant landuse type
c
 101  continue
      do j = 1,ny
        do i = 1,nx
          fmax = 0.
          lumax(i,j) = 0
          sum = 0.
          do n = 1,11
            fmax = amax1(fland(i,j,n),fmax)
            if (fmax.eq.fland(i,j,n) .and. fland(i,j,n).ne.0.)
     &        lumax(i,j) = n
            sum = sum + fland(i,j,n)
          enddo
          if (sum.lt.0.99.or.sum.gt.1.01) then
              write(*,*)'total <> 1 ',i,j,sum
              sum_off = .true.
          endif
        enddo
      enddo

c
c-----Determine return value 
c
      if (sum_off) then
      	call exit(1)
      else
      	call exit(0)
      endif

      stop
      end
