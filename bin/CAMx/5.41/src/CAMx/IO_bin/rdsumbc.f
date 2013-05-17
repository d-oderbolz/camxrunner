      subroutine rdsumbc(idtnow,timnow,jdlast,ttlast,
     &                                   ncolx,nrowy,nlays,consum)
      use filunit
      use grid
      use chmstry
      use bndary
      use camxcom
      use tracer
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c
c----CAMx v5.41 121109
c
c     11/20/03   --gwilson--    Fixed bug when using the species map
c                               to get the index of modeled species
c     11/02/05   --gwilson--    Added common block for large local
c                               arrays that were causing segfault in
c                               OMP mode.
c     10/28/09   --gwilson--    Made the large local arrays
c                               allocatable to avoid memory issues
c     11/4/09    -cemery-       Removed input top concentrations
c
c
      include 'camx.prm'
c
      character*4 iname(10)
      real        consum(MXSPEC,0:IDXBTP)
      logical     lfound
      integer     ncola, nrowa, nlaya
c
      real, allocatable, dimension(:) :: height
      real, allocatable, dimension(:) :: depth

      real conwst(MXLAYER,MXCELLS)
      real conest(MXLAYER,MXCELLS)
      real consth(MXLAYER,MXCELLS)
      real connth(MXLAYER,MXCELLS)
c
c---- Entry point ---
c
c  --- allocate the local arrays ---
c
      ncola = maxval( ncol(1:ngrid) )
      nrowa = maxval( nrow(1:ngrid) )
      nlaya = maxval( nlay(1:ngrid) )
      allocate( height(ncola*nrowa*nspec) )
      allocate( depth(ncola*nrowa*nspec) )
c
      ibgdhp = 0
      btimhp = 0 
      ibegbc = 0
      btimbc = 0.
      iendbc = 0
      etimbc = 0.
c
c   --- initialize the array to zero ---
c
      do i=1,nspec
        do j=0,IDXBTP 
          consum(i,j) = 0.
        enddo
      enddo
c
c   --- read the ZP file to get the layer heights ----
c
      rewind(ihtp(1))
  333 continue
c
      call getdepth(ncolx,nrowy,nlays,ly2k,ibgdhp,idtnow,btimhp,timnow,
     &              ihtp(1),height,depth)
c
c   --- read boundary conditions for this hour ----
c
      read(ibc,ERR=7002) ibegbc, btimbc, iendbc, etimbc
      lfound = .TRUE.
      if( ibegbc .EQ. idtnow .AND. btimbc .GT. timnow ) lfound = .FALSE.
      if( etimbc .EQ. 0. ) then
         iendbc = iendbc - 1
         etimbc = 24.0
      endif
      if( ibegbc .EQ. idtnow .AND. etimbc .LT. timnow ) lfound = .FALSE.
      if( ibegbc .EQ. idtnow .AND. iendbc .GT. idtnow ) lfound = .TRUE.
      if( ibegbc .GT. idtnow ) lfound = .FALSE.
      do 70 ispc=1,nbcspc
          read(ibc,ERR=7002,END=7003) iseg, (iname(i),i=1,10), iedge,
     &                         ((conwst(izcl,j),izcl=1,nlays),j=1,nrowy)
          read(ibc,ERR=7002,END=7003) iseg, (iname(i),i=1,10), iedge,
     &                         ((conest(izcl,j),izcl=1,nlays),j=1,nrowy)
          read(ibc,ERR=7002,END=7003) iseg, (iname(i),i=1,10), iedge,
     &                         ((consth(izcl,i),izcl=1,nlays),i=1,ncolx)
          read(ibc,ERR=7002,END=7003) iseg, (iname(i),i=1,10), iedge,
     &                         ((connth(izcl,i),izcl=1,nlays),i=1,ncolx)
c
c   --- if record does not span this hour, skip it ----
c
          if( .NOT. lfound ) goto 70
c
c   --- if the species is a not modelled or not a VOC species skip it ---
c
          idx = 0
          do i=1,nspec
            if( lbcmap(i) .EQ. ispc ) idx = i
          enddo
          if( idx .LE. 0 ) goto 70
          if( .NOT. (lvocsp(idx) .OR. lvocsoa(idx)) ) goto 70
          call sumwt4(idx,ncolx,nrowy,nlays,depth,conwst,conest,
     &                                          consth,connth,consum)
c
c   --- if record does not span this hour, skip it ----
c
c   --- next species ---
c
   70 continue
c
c   --- chack date and time, if it is still in the episode
c       go back to read the next hour ----
c
      if( lfound ) then
         timnow = timnow + 1.0
         if( timnow .GT. 24.0 ) then
             idtnow = idtnow + 1
             timnow = 0.0
             if( MOD(idtnow,1000) .GT. 365 ) then
                if( MOD(INT(idtnow/1000),4) .EQ. 0 ) then
                   if( MOD(idtnow,1000) .EQ. 367 )
     &                     idtnow = (INT(idtnow/1000)+1)*1000 + 1
                else
                   idtnow = (INT(idtnow/1000)+1)*1000 + 1
                endif
             endif
         endif
      endif
      if( idtnow .GT. jdlast .OR. 
     &          (idtnow .EQ. jdlast .AND. timnow .GE. ttlast) ) goto 444
c
c  --- check if we need to back up the boundary conditions file, that is
c      does the record just read contain this hour ----
c
      if( idtnow .LT. iendbc .OR. 
     &           (idtnow .EQ. iendbc .AND.  timnow .LT. etimbc ) ) then
         do 81 i=1,nbcspc*4
            backspace(ibc)
   81    continue
         backspace(ibc)
      endif
c
c  --- process next hour ----
c
      goto 333
c
c  --- all concentrations are summed, return ---
c
  444 continue
c
c  --- deallocate the local arrays ---
c
      deallocate( height )
      deallocate( depth )
c
      ibgdhp = 0
c
c   ---- rewind files to be used again by the regular model ---
c
      rewind(ibc)
      rewind(ihtp(1))
      return
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in RDSUMBC:'
      write(iout,'(/,1X,A)') 'Reading boundary conditions. '
      call camxerr()
c
 7003 continue
      write(iout,'(//,a)') 'ERROR in RDSUMBC:'
      write(iout,'(/,1X,2A)') 'Premature end-of-file reading ',
     &                                         'boundary conditions.'
      call camxerr()
      end
