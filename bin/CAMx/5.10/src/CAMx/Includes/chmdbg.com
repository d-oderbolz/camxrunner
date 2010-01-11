c----CAMx v5.10 090918
c  
c     CHMDBG.COM contains the debug variables for the Chemistry routines
c                            
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c            
c     Modifications:  
c 
c-----------------------------------------------------------------------
c     Variables for keeping track of where chmistry is being performed:
c     NOTE:  Used for diagnostic and error messages.
c
c     igrdchm  --  grid number of current chemistry step
c     ichm     --  column for the current chemistry step
c     jchm     --  row for the current chemistry step
c     kchm     --  layer for the current chemistry step
c     tchm     --  temperature in current chemistry step
c     wchm     --  water in current chemstry step
c     ldchm    --  ldark flag in current chemistry step
c
c-----------------------------------------------------------------------
c
      integer   igrdchm
      integer   ichm
      integer   jchm
      integer   kchm
      real      tchm
      real      wchm
      logical   ldchm
c
      common /ijkgrd/ igrdchm, ichm, jchm, kchm, tchm, wchm, ldchm
c$omp threadprivate(/ijkgrd/)
c
