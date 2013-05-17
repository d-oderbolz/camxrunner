      logical function isbound(ibcon,m1,m2,i,j)   
c
      implicit none
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        Test if this point is on a real boundary
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2012
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
      integer :: ibcon
      integer :: m1
      integer :: m2
      integer :: i
      integer :: j
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      isbound = .false.
c
      if (btest(ibcon,0) .and. i ==  1) then
         isbound = .true.
         return
      elseif (btest(ibcon,1) .and. i == m1) then
         isbound = .true.
         return
      elseif (btest(ibcon,2) .and. j ==  1) then
         isbound = .true.
         return
      elseif (btest(ibcon,3) .and. j == m2) then
         isbound = .true.
         return
      endif
c
      return
      end
