c**** IS1ALPHA
c
      function is1alpha( string )
      logical is1alpha
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Test whether the first character of a string is alphabetical
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c    Argument description:
c     Inputs:
c      string  C  string to test
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*52 alphabet
c
c-----------------------------------------------------------------------
c    Data statments:
c-----------------------------------------------------------------------
c
      data alphabet 
     &         /'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      is1alpha = .false.
      if ( INDEX( alphabet, string(1:1) ) .GT. 0 ) is1alpha = .true.
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
