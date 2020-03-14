      program pigreco
      implicit none
      include "mpif.h"

      CHARACTER(100) sintervals
      INTEGER :: i, start, fin, intervals
      INTEGER :: ierr, nproc, rank
      REAL :: mypi, pi
      REAL, parameter :: PI25DT = acos(-1.d0)

      CALL MPI_INIT(ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

      if(rank .eq. 0) then
         write(6,*) "MPI version with tastks = ", nproc
      endif

      CALL GET_COMMAND_ARGUMENT(1, sintervals)
      READ(sintervals,*)intervals
      intervals = intervals / nproc

      mypi = 0.d0
      start = rank * intervals
      fin = (rank + 1) * intervals
      do i = start, fin
          mypi = mypi + (-1.d0)**REAL(i)/(2.d0 * REAL(i) + 1.d0)
      enddo

      CALL MPI_Reduce(mypi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

      pi = 4.d0 * pi

      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

      if(rank .eq. 0) then
         write(6,*)' Computed PI =', pi
         write(6,*)' The True PI =', PI25DT
         write(6,*)' Error        ', PI25DT - pi
      endif

      CALL MPI_FINALIZE(ierr)

      end program

