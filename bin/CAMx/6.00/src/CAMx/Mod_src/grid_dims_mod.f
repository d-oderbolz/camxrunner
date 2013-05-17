      Module grid_dims                 
c
c----CAMx v6.00 130506
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
c       NXPMAX=1000   I  Maximum number of points in x-direction 
c       NYPMAX=1000   I  Maximum number of points in y-direction
c       NZPMAX=132    I  Maximum number of points in z-direction
c     Output:  
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
         integer, parameter :: MAXGRDS=10
         integer, parameter :: NXPMAX=1000
         integer, parameter :: NYPMAX=1000
         integer, parameter :: NZPMAX=132
c
c        maximum number of processors that can be used in a parallel run
c
         integer, parameter :: MAXMACH=256
c
         integer, parameter :: MAXDIM2=1000*2
c
      End Module grid_dims
