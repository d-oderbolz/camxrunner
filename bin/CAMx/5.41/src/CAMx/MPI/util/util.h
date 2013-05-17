#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>
#include <mpi.h>

#define MAX_MSGTAG 1000

#define MPI_request int

int flag_msgtag;

MPI_Request mpi_msgtags[MAX_MSGTAG];

char *ibuff;
int ipos,nbuff;

#endif
