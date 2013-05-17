      subroutine find_edge()
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use master_mod
      use node_mod
c
      implicit none
c
c----CAMx v5.41 121109
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
c       NODES_ALLOC
c    Subroutines called:
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      logical, external :: isbound
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer imach
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- loop over number of machines ---
c
      do imach=1,nmachs
         nodeedge(imach) = .FALSE.
c
c   --- North edge ---
c
        if( isbound( ibcflg(imach,1),
     &            nxend(imach,1),nyend(imach,1),0,nyend(imach,1) ) )
     &                                        nodeedge(imach) = .TRUE.
c
c   --- South edge ---
c
        if( isbound( ibcflg(imach,1),
     &                    nxend(imach,1),nyend(imach,1),0,1 ) )
     &                                     nodeedge(imach) = .TRUE.
c
c   --- West edge ---
c
        if( isbound( ibcflg(imach,1),
     &                    nxend(imach,1),nyend(imach,1),1,0 ) )
     &                                     nodeedge(imach) = .TRUE.
c
c   --- East edge ---
c
        if( isbound( ibcflg(imach,1),
     &         nxend(imach,1),nyend(imach,1),nxend(imach,1),0 ) )
     &                                     nodeedge(imach) = .TRUE.
c
      enddo
      end
