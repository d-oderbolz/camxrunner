c**** TRACERPTS
c
c----CAMx v5.41 121109
c
c
c----------------------------------------------------------------------
c
c    Include file for tracer parameters and data structures used
c    in the source apportionment version of the CAMx. This file is
c    just for the point source array.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   sapnts    R   array of tracer emissions for each point source
c                   
      real      sapnts(MXPTSRC,MXTRSP)
c
      common /sappdat/ sapnts
