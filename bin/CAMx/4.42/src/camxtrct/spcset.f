c**** SPCSET.F
c
      subroutine spcset(ierr)
c
c-----------------------------------------------------------------------
c
c   This routine looks at ther user specied species name and sets the
c   flags for determining which model species should be extracted.  This
c   data is stored in common to be used by the CAMXtrct program.
c     Argument description:
c      Outputs:
c        ierr   I  error code
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camxtrct.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c  MXVOC  I  maximum number of VOC species
c  MXNOX  I  maximum number of NOx species
c  MXNOY  I  maximum number of NOy (all Nitrogen species)
c
      integer*4 MXVOC
      integer*4 MXNOX
      integer*4 MXNOY
c
      parameter( MXVOC = 11 )
      parameter( MXNOX =  2 )
      parameter( MXNOY =  7 )
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
      integer*4 strlen
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 splow, spup, spcvoc(MXVOC), spcnox(MXNOX)
      character*10 spcnoy(MXNOY)
      integer*4    islash, ncarb(MXVOC), idxvoc, idxnox, idxlow, idxup
      integer*4    idxnoy, ilen, i
      logical*4    llow, lfound
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data spcvoc /'ETH       ','OLE       ','PAR       ','TOL       ',
     &             'XYL       ','FORM      ','ALD2      ','ISOP      ',
     &             'ETOH      ','MEOH      ','ISPD      '/
      data ncarb  / 2          , 2          , 1          , 7          ,
     &              8          , 1          , 2          , 5          ,
     &              2          , 1          , 5          /
      data spcnox /'NO        ','NO2       '/
      data spcnoy /'NO        ','NO2       ','PAN       ','HNO2      ',
     &             'HNO3      ','NXOY      ','NTR       '/
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- initialize the error code ---
c
      ierr = IFAIL
c
c  --- initialize some common variables ---
c
       lratio = .FALSE.
       do 10 i=1,nspec
         lnumer(i) = .FALSE.
         ldenom(i) = .FALSE.
   10  continue
c
c  --- check for a slash in the species name (determines a ratio) ---
c
      islash = INDEX( spname,'/' )
      if( islash  .GT. 0 ) then
         llow = .TRUE.
         spup = spname(1:islash-1)
         splow = spname(islash+1:strlen(spname))
      else
         llow = .FALSE.
         spup = spname
      endif
c
c  --- check for exact match of spcies names --
c
      idxup = fndchr( spup, 10, spclst, nspec ) 
      if( idxup .GT. 0 ) then
         lnumer(idxup) = .TRUE.
         facnum(idxup) = 1.0
      endif
      if( llow ) then
         idxlow = fndchr( splow, 10, spclst, nspec ) 
         if( idxlow .GT. 0 ) then
            ldenom(idxlow) = .TRUE.
            lratio = .TRUE.
            facden(idxlow) = 1.0
         endif
      endif
c
c  --- if matched exact species, then set error flag and return ---
c
      if( .NOT. llow .AND. idxup .GT. 0 ) goto 111
      if( llow .AND. idxup .GT. 0 .AND. idxlow .GT. 0 ) goto 111
c
c  --- check numerator for VOC ---
c
      if( spup .EQ. 'VOC' .OR. spup .EQ. 'RHC' ) then
         do 20 i=1,nspec
            idxvoc = fndchr(spclst(i), 10, spcvoc, MXVOC )
            if( idxvoc .GT. 0 ) then
               lnumer(i) = .TRUE.
               facnum(i) = FLOAT( ncarb(idxvoc) )
            endif
   20    continue
      endif
c
c  --- check denominator for VOC ---
c
      if( llow ) then
         if( splow .EQ. 'VOC' .OR. splow .EQ. 'RHC' ) then
            do 30 i=1,nspec
               idxvoc = fndchr(spclst(i), 10, spcvoc, MXVOC )
               if( idxvoc .GT. 0 ) then
                  ldenom(i) = .TRUE.
                  lratio = .TRUE.
                  facden(i) = FLOAT( ncarb(idxvoc) )
               endif
   30       continue
         endif
      endif
c
c  --- check numerator for NOX ---
c
      if( spup .EQ. 'NOX' ) then
         do 40 i=1,nspec
            idxnox = fndchr(spclst(i), 10, spcnox, MXNOX )
            if( idxnox .GT. 0 ) then
               lnumer(i) = .TRUE.
               facnum(i) = 1.0
             endif
   40    continue
      endif
c
c  --- check denominator for NOX ---
c
      if( llow ) then
         if( splow .EQ. 'NOX' ) then
            do 50 i=1,nspec
               idxnox = fndchr(spclst(i), 10, spcnox, MXNOX )
               if( idxnox .GT. 0 ) then
                  ldenom(i) = .TRUE.
                  lratio = .TRUE.
                  facden(i) = 1.0
               endif
   50       continue
         endif
      endif
c
c  --- check numerator for NOY ---
c
      if( spup .EQ. 'NOY' ) then
         do 45 i=1,nspec
            idxnoy = fndchr(spclst(i), 10, spcnoy, MXNOY )
            if( idxnoy .GT. 0 ) then
               lnumer(i) = .TRUE.
               facnum(i) = 1.0
             endif
   45    continue
      endif
c
c  --- check denominator for NOY ---
c
      if( llow ) then
         if( splow .EQ. 'NOY' ) then
            do 55 i=1,nspec
               idxnoy = fndchr(spclst(i), 10, spcnoy, MXNOY )
               if( idxnoy .GT. 0 ) then
                  ldenom(i) = .TRUE.
                  lratio = .TRUE.
                  facden(i) = 1.0
               endif
   55       continue
         endif
      endif
c
c  --- check numerator for T in first charcater (indicates Total tracer) ----
c
      if( spup(1:1) .EQ. 'T' ) then
         ilen = strlen( spup )
         do 60 i=1,nspec
c
c  --- only check the part of string equal to length of users name ----
c
            if( spclst(i)(1:ilen-1) .EQ. spup(2:ilen) ) then 
               lnumer(i) = .TRUE.
               facnum(i) = 1.0
            endif
   60    continue
       endif
c
c  --- check numerator for T in first characater (indicates Total tracer) ----
c
      if( llow ) then
         if( splow(1:1) .EQ. 'T' ) then
            ilen = strlen( splow )
            do 70 i=1,nspec
c
c  --- only check the part of string equal to length of users name ----
c
               if( spclst(i)(1:ilen-1) .EQ. splow(2:ilen) ) then 
                  ldenom(i) = .TRUE.
                  lratio = .TRUE.
                  facden(i) = 1.0
               endif
   70       continue
          endif
       endif
c
c  --- check numerator for O3R, IR, or DR (indicates tracer region) ----
c
      if( spup(1:3) .EQ. 'O3R' .OR. spup(1:2) .EQ. 'IR' .OR.
     &                                     spup(1:2) .EQ. 'DR') then
         do 80 i=1,nspec
c
c  --- only check the approprate part of string ----
c
            if( spup(1:1) .EQ. 'O' .AND. spclst(i)(1:2) .EQ. 'O3') then
               if( spup(7:7) .EQ. 'E' ) then
                  if( spclst(i)(7:9) .EQ. spup(4:6) .AND. 
     &                            spclst(i)(4:6) .EQ. spup(8:10) ) then 
                     lnumer(i) = .TRUE.
                     facnum(i) = 1.0
                  endif
               else
                  if( spclst(i)(7:9) .EQ. spup(4:6) ) then 
                     lnumer(i) = .TRUE.
                     facnum(i) = 1.0
                  endif
               endif 
            else
               if( spclst(i)(7:9) .EQ. spup(3:5) .AND. 
     &                         spclst(i)(1:1) .EQ. spup(1:1)) then 
                  lnumer(i) = .TRUE.
                  facnum(i) = 1.0
               endif
            endif
   80    continue
       endif
c
c  --- check denominator for O3R, IR, or DR (indicates tracer region) ----
c
      if( llow ) then
         if( splow(1:3) .EQ. 'O3R' .OR. splow(1:2) .EQ. 'IR' .OR.
     &                                     splow(1:2) .EQ. 'DR') then
            do 90 i=1,nspec
c
c  --- only check the approprate part of string ----
c
               if( splow(1:1) .EQ. 'O' .AND. 
     &                                spclst(i)(1:2) .EQ. 'O3') then
                  if( splow(7:7) .EQ. 'E' ) then
                     if( spclst(i)(7:9) .EQ. splow(4:6) .AND.
     &                           spclst(i)(4:6) .EQ. splow(8:10) ) then 
                        ldenom(i) = .TRUE.
                        lratio = .TRUE.
                        facden(i) = 1.0
                     endif
                  else
                     if( spclst(i)(7:9) .EQ. splow(4:6) ) then
                        ldenom(i) = .TRUE.
                        lratio = .TRUE.
                        facden(i) = 1.0
                     endif
                  endif
                else
                   if( spclst(i)(7:9) .EQ. splow(3:5) ) then 
                     ldenom(i) = .TRUE.
                     lratio = .TRUE.
                     facden(i) = 1.0
                  endif
               endif
   90       continue
        endif
      endif
c
c  --- check numerator for E and O3 (indicates emissions group) ----
c
      if( spup(1:1) .EQ. 'E' .AND. spup(5:6) .EQ. 'O3') then
         do 11 i=1,nspec
c
c  --- only check the part of string equal to length of users name ----
c
c
c  --- skip the non-emissions species ---
c
            if( spclst(i)(7:8) .EQ. 'IC' ) goto 11
            if( spclst(i)(7:8) .EQ. 'BC' ) goto 11
            if( spclst(i)(1:2) .EQ. 'O3' 
     &                     .AND. spclst(i)(4:6) .EQ. spup(2:4) ) then
               if( spup(7:7) .EQ. 'T' .OR. 
     &                              spup(7:7) .EQ. spclst(i)(3:3) ) then
                  lnumer(i) = .TRUE.
                  facnum(i) = 1.0
               endif 
            endif
   11    continue
      endif
c
c  --- check demonimator for E and O3 (indicates emissions group) ----
c
      if( llow ) then
         if( spup(1:1) .EQ. 'E' .AND. spup(5:6) .EQ. 'O3') then
            do 21 i=1,nspec
c
c  --- skip the non-emissions species ---
c
               if( spclst(i)(7:8) .EQ. 'IC' ) goto 21
               if( spclst(i)(7:8) .EQ. 'BC' ) goto 21
               if( spclst(i)(1:2) .EQ. 'O3' 
     &                     .AND. spclst(i)(4:6) .EQ. spup(2:4) ) then
                  if( spup(7:7) .EQ. 'T' .OR. 
     &                              spup(7:7) .EQ. spclst(i)(3:3) ) then
                     lnumer(i) = .TRUE.
                     facnum(i) = 1.0
                  endif 
               endif
   21       continue
         endif
      endif
c
c   --- echo species being used ---
c
  111 continue
      write(IOWSTD,'(/,T30,A)') 'Species Being Used in Extraction'
      write(IOWSTD,'(10X,A,6X,A)') 'Name',
     &           'Factor (negative indicates denominator of ratio)'
      lfound = .FALSE.
      do 31 i=1,nspec
         if( lnumer(i) ) then
            write(IOWSTD,'(10X,A,8X,F10.1)') spclst(i), facnum(i)
            lfound = .TRUE.
         else if( ldenom(i) ) then    
            write(IOWSTD,'(10X,A,8X,F10.1)') spclst(i), -facden(i)
            lfound = .TRUE.
         endif
  31  continue
      if( .NOT. lfound ) goto 7000
      write(IOWSTD,'(A)') 
c
c   --- set error code and return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(/,A)') 'ERROR:  No species matched in file. '
      write(IOWSTD,'(/,3X,A)') 
     &                   'Sums of species supported are: NOX, NOY, VOC'
      write(IOWSTD,'(/,3X,2A)') 'Sums of tracer species supported ',
     &                               'are: TNOX, TVOC, TO3, TO3N, TO3V'
      write(IOWSTD,'(/,3X,2A)') 'Tracer species by region: ',
     &                                          'O3Rnnn, IRnnn, DRnnn '
      write(IOWSTD,'(16X,A)') 'where nnn is the region'
      write(IOWSTD,'(/,3X,2A)') 'Tracer species by emissions group: ',
     &                                     'EnnnO3N, EnnnO3V, EnnnO3T '
      write(IOWSTD,'(/,3X,2A)') 'Tracer species by region/group: ',
     &                                          'O3RnnnEmmm'
      write(IOWSTD,'(16X,A)') 'where nnn is the region, '
      write(IOWSTD,'(16X,A)') 'and mmm is the emissions group'
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
