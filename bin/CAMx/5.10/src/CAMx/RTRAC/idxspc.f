C**** IDXSPC
c
c----CAMx v5.10 090918
c
c    Copyright 1996-2006
c    ENVIRON International Corporation
c
      function idxspc(spcnam, spclst, namlen, nname)
      integer  idxspc
c
c-----------------------------------------------------------------------
c
c     This function returns the position index of SPCNAM in SPCLST
c     Return the first match if there are multiple matches
c     Return zero if not matched
c
c    Arguments:
c     Inputs:
c       spcnam   C   string to search for
c       spclst   C   array of strings to search through
c       namlen   I   string length
c       nname    I   number of array elements to search 
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      integer       nname, namlen
      character*(*) spcnam, spclst(nname)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer       i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      idxspc = 0
      if( nname .LT. 1 ) goto 9999
      do i = 1,nname
        if( spcnam .EQ. spclst(i) ) then
           idxspc = i
           goto 9999
        endif
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

