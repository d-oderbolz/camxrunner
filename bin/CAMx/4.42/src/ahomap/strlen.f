      function strlen(
     &                 string, idclar )
      integer*4 strlen
c
c-----------------------------------------------------------------------
c
c   description:
c
c     this routine returns the actual length of a string, i.e. with no
c     trailing BLANKs.
c
c   Arguments:
c
c     Inputs:
c       string   C   string for determining length
c       idclar   C   declared length of string
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     11/10/91  -gmw-  original development
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
      integer*4     idclar
c
c-----------------------------------------------------------------------
c   local parameters:
c-----------------------------------------------------------------------
c
c       BLANK    C  BLANK character
c
      character*1 BLANK
      parameter( BLANK=' ' )
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize length to zero ----
c
      strlen = 0
c
c   ---- check from end of string to beginning for non-BLANK ----
c
      do 10 i=idclar,1,-1
         if( string(i:i) .NE. BLANK ) then
c
c   ---- non-BLANK character found, return length ----
c
             strlen = i
             goto 9999
         endif
   10 continue
c
c   ---- string must be all BLANK, return the zero length ----
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
