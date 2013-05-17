      subroutine myoffset(atype,a,nestd,b)
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       NODE_GET_NBC
c       NODE_GET_NBC_PT
c       NODE_SEND_FEED
c       NODE_SEND_FEED_PT
c       NODE_SEND_NBC
c       NODE_SEND_NBC_PT
c    Subroutines called:
c       
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: atype
      integer :: a
      integer :: nestd
      integer :: b
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (a .le. 1) then
         stop 'offset exception for nesting domain'
c
c  --- begin type ---
c
      elseif (atype .eq. 0) then
         b = mod((a-2), nestd)
c
c  --- end type ---
c
      elseif (atype .eq. 1) then
         b = nestd - mod((a-2), nestd) - 1
      endif
c
      return
      end 
