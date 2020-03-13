
      program pi_mpi
      implicit none

      include "mpif.h"
      INTEGER :: i, j, ierr
      INTEGER :: nproc, rank
      INTEGER, ALLOCATABLE :: arr(:)
      INTEGER status(MPI_STATUS_SIZE)

      CALL MPI_INIT(ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

      write(6,*)"Hello, ass!"

      CALL MPI_FINALIZE(ierr)


100   format(100i4)
      end


