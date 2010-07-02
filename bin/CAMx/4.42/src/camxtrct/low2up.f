      subroutine low2up( string )
c
c-----------------------------------------------------------------------
c
c   Description:
c
c     This routine converts a string from lower case to upper case.
c
c   Arguments:
c
c     Inputs/Outputs: (the string arguments serves as both input and
c                      output)
c       string   C   string to convert
c
c-----------------------------------------------------------------------
c   Log:
c-----------------------------------------------------------------------
c
c     11/10/91  -gmw-  original development
c     03/20/92  -gmw-  changed the algorithm because it is not portable
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idiff, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- check from beninng of string to end for non-blank ----
c
      do 10 i=1,LEN( string )
        idiff = ICHAR( string(i:i) ) - ICHAR( 'a' )
        if( idiff .GE. 0 .AND. idiff .LE. 26 ) then
            string(i:i) = CHAR( ICHAR( 'A' ) + idiff )
        endif
   10 continue
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
