      Module grid_dims                 
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This module contains very basic specification of grid dimensions and other 
c        parameters that will be used to dimension arrays and allocate memory.
c-----------------------------------------------------------------------
c
c    Parameter descriptions:
c     Input:
c       MAXGRDS=10    I  Maximum number of grids
c       NXPMAX=303    I  Maximum number of points in x-direction 
c       NYPMAX=303    I  Maximum number of points in y-direction
c       NZPMAX=132    I  Maximum number of points in z-direction
c     Output:  
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
         integer, parameter :: MAXGRDS=10
         integer, parameter :: NXPMAX=303
         integer, parameter :: NYPMAX=303
         integer, parameter :: NZPMAX=132
c
c        maximum number of processors that can be used in a parallel run
c
         integer, parameter :: MAXMACH=64
c
         integer, parameter :: MAXDIM2=303*2
c
      End Module grid_dims
