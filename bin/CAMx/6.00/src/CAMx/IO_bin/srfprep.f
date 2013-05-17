      subroutine srfprep(igrd,ncol,nrow,orgx,orgy,dxmod,dymod,
     &                   fsurf,topo,lai,lrdlai,albedo)
      use filunit
      use bndary
      use camx_includes
c
c----CAMx v6.00 130506
c
c     SRFPREP reads the surface/landuse files for all grids and initializes the
c     landuse field arrays.  Landuse is mapped to all nested grids that
c     are not supplied with a surface file
c                          
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c          
c     Modifications:
c        6/6/03       check input data for consistency
c        7/7/03       now skips everthing if file not supplied
c        4/27/06      Added read of optional topo field
c        7/5/07       Added read of optional LAI field
c        1/04/11      Revised for new met input format
c        02/11/11     Albedo codes are now set here (not in AHOPREP)
c
c     Input arguments:
c        igrd                grid index
c        ncol                number of columns
c        nrow                number of rows
c        orgx                grid x-origin (km or deg)
c        orgy                grid y-origin (km or deg)
c        dxmod               grid spacing in x-direction (km or deg)
c        dymod               grid spacing in y-direction (km or deg)
c
c     Output arguments:
c        fsurf               fractional landuse field
c        topo                topographic elevation (m MSL, optional)
c        lai                 lead area index field (optional)
c        lrdlai              flag indicating if LAI was read
c        albedo              surface albedo as f(LU)
c
c     Routines Called:
c        JSTLFT
c        TOUPPER
c
c     Called by:
c        STARTUP
c
      implicit none
c
      integer igrd,ncol,nrow
      real orgx,orgy,dxmod,dymod
      real fsurf(ncol,nrow,nlu),topo(ncol,nrow),lai(ncol,nrow)
      real albedo(ncol,nrow)
      logical lrdlai
c
      character*10 namsrf(NLUZ03),namevar(MXVAR)
      character*4 namvar(10)
      integer i,j,iunit,m,n,l,idum,i1,i2
      integer lumap(NLUZ03)
      integer mapvar(NLUZ03)
      real areatot,alb
      real arr2d(MXCELLS,MXCELLS,MXVAR)
      real albcw(NLUW89),albcz(NLUZ03)
c
      data namsrf   /'WATER     ',
     &               'ICE       ',
     &               'LAKE      ',
     &               'ENEEDL    ',
     &               'EBROAD    ',
     &               'DNEEDL    ',
     &               'DBROAD    ',
     &               'TBROAD    ',
     &               'DDECID    ',
     &               'ESHRUB    ',
     &               'DSHRUB    ',
     &               'TSHRUB    ',
     &               'SGRASS    ',
     &               'LGRASS    ',
     &               'CROPS     ',
     &               'RICE      ',
     &               'SUGAR     ',
     &               'MAIZE     ',
     &               'COTTON    ',
     &               'ICROPS    ',
     &               'URBAN     ',
     &               'TUNDRA    ',
     &               'SWAMP     ',
     &               'DESERT    ',
     &               'MWOOD     ',
     &               'TFOREST   '/
      data albcw/.08,.05,.05,.05,.05,.05,.04,.08,.05,.05,.05/
      data albcz/.04,.50,.04,.05,.05,.05,.05,.05,.05,.05,
     &           .05,.05,.05,.05,.05,.05,.05,.05,.05,.05,
     &           .08,.05,.05,.08,.05,.05/
      data lumap / 7, 8, 7, 5, 5, 4, 4, 4, 4, 3,
     &             3, 3, 3,10, 2, 2, 2, 2, 2, 2,
     &             1, 9, 9, 8, 6, 4/
c
c-----Entry point
c
      lrdlai = .false.
c
c-----Skip if file not provided
c
      do j = 1,nrow
        do i = 1,ncol
          topo(i,j) = 0.
          lai(i,j)  = 0.
        enddo
      enddo
      iunit = isurf(igrd)
      if (iunit .LE. 0) goto 9999
c
c-----Read surface file header
c
      call rdmethdr(iunit,'SURFACE   ',igrd,0.,0,0.,0,ncol,nrow,1,
     &              orgx,orgy,dxmod,dymod,iout,nsrfvar(igrd))
c
c-----Read variable fields
c
      read(iunit,end=7000)
      do n = 1,nsrfvar(igrd)
        read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                       ((arr2d(i,j,n),i=1,ncol),j=1,nrow)
        write(namevar(n),'(10a1)') (namvar(m),m=1,10)
      enddo
      write(iout,'(a40,15x,a,i3)') 'Read LU field',' grid',igrd
c
c-----Load landuse fractions depending on selected dry dep algorithm
c
      do n = 1,nlu
        do j = 1,nrow
          do i = 1,ncol
            fsurf(i,j,n) = 0.
          enddo
        enddo
      enddo
      do n = 1,NLUZ03
        do l = 1,nsrfvar(igrd)
          if (namevar(l).eq.namsrf(n)) then
            mapvar(n) = l
          endif
        enddo
      enddo
      do n = 1,NLUZ03
        do j = 1,nrow
          do i = 1,ncol
            if (idrydep.eq.2) then
              fsurf(i,j,n) = arr2d(i,j,mapvar(n))
            else
              fsurf(i,j,lumap(n)) = fsurf(i,j,lumap(n)) + 
     &                              arr2d(i,j,mapvar(n))
            endif
          enddo
        enddo
      enddo
c
      do l = 1,nsrfvar(igrd)
        if (namevar(l).eq.'LAI') then
          do j = 1,nrow
            do i = 1,ncol
              lai(i,j) = arr2d(i,j,l)
            enddo
          enddo
          write(iout,'(a40,15x,a,i3)') 'Read LAI field',' grid',igrd
          lrdlai = .true.
        elseif (namevar(l).eq.'TOPO_M') then
          do j = 1,nrow
            do i = 1,ncol
              topo(i,j) = arr2d(i,j,l)
            enddo
          enddo
          write(iout,'(a40,15x,a,i3)') 'Read TOPO field',' grid',igrd
        endif
      enddo
c
c-----Check that the LU data are reasonable and adjust out
c     minor inconsistencies
c
      do j = 1,nrow
        do i = 1,ncol
           areatot = 0.0
           do l = 1,nlu
             areatot = areatot + fsurf(i,j,l)
           enddo
           if (areatot.lt.0.95 .or. areatot.gt.1.05) goto 7001
           do l = 1,nlu
             fsurf(i,j,l) = fsurf(i,j,l)/areatot
           enddo
         enddo
      enddo
c10   continue
c
c-----Assign albedo codes from LU field
c
      do j = 1,nrow
        do i = 1,ncol
          alb = 0.
          if (nlu .EQ. 11) then
            do l = 1,nlu
              alb = alb + fsurf(i,j,l)*albcw(l)
            enddo
          else
            do l = 1,nlu
              alb = alb + fsurf(i,j,l)*albcz(l)
            enddo
          endif
          albedo(i,j) = alb
        enddo
      enddo
c
 9999 return
c
c-----Error in landuse fractions
c
 7000 continue
      write(iout,'(//,a)')'ERROR in SRFPREP:'
      write(iout,'(A,I5)') 'Reading surface file for grid:',igrd
      write(iout,*)'End of input file reached.  Make sure the file '
      write(iout,*)'is in the correct format and contains: ',
     &              nsrfvar(igrd)
      write(iout,*)'variable fields.'
      write(iout,*)
      call camxerr()
c
 7001 write(iout,'(//,A)') 'ERROR in SRFPREP'
      write(iout,'(/,2A)') 'Sum of landuse fractions differs from 1.0 ',
     &                     'by more than 5%'
      write(iout,'(A,i3,a,2i4)') 
     &       'Grid = ', igrd, '  Cell(i,j) = ', i, j
      write(iout,'(A)') 'Table of input landuse data follows:'
      write(iout,'(/,A)') ' Class    Fraction'
      write(iout,'(i6,F10.3)') (l,fsurf(i,j,l),l=1,nlu)
      write(iout,'(A6,F10.3)') 'Total', areatot
      write(iout,'(/,A)') 'Check your input landuse data file'
      call camxerr()
c
      end
