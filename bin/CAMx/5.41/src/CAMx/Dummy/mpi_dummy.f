      subroutine MPI_BARRIER(COMM,IERROR)
      integer COMM
      integer IERROR
      end

      subroutine MPI_COMM_RANK(COMM,RANK,IERROR)
      integer COMM
      integer IERROR
      integer RANK
      end

      subroutine MPI_COMM_SIZE(COMM,SIZE,IERROR)
      integer COMM
      integer IERROR
      integer SIZE
      end

      subroutine MPI_FINALIZE(IERROR)
      integer IERROR
      end

      subroutine MPI_INIT(IERROR)
      integer IERROR
      IERROR = 1
      end

      subroutine MPI_SEND(BUF,COUNT,DATATYPE,DEST,TAG,COMM,IERROR)
      real    BUF
      integer COUNT
      integer DATATYPE
      integer DEST
      integer TAG
      integer COMM
      integer IERROR
      end

      subroutine MPI_RECV(BUF,COUNT,DATATYPE,SOURCE,TAG,COMM,
     &                                              STATUS,IERROR)
      real    BUF
      integer COUNT
      integer DATATYPE
      integer TAG
      integer COMM
      integer STATUS
      integer IERROR
      end

      subroutine MPI_ABORT(COMM,RANK,IERROR)
      integer COMM
      integer IERROR
      integer RANK
      end

      subroutine MPI_NULL_COPY_FN()
      end

      subroutine MPI_NULL_DELETE_FN()
      end

      subroutine MPI_DUP_FN()
      end
