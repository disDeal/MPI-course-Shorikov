      program pigreco
      implicit none
      include "mpif.h"

      CHARACTER(100)sintervals
      INTEGER(selected_int_kind(18)) :: i, istart, iend
      INTEGER(selected_int_kind(18)) :: intervals
      INTEGER:: ierr, nproc, myrank
      REAL(kind(1.d0)) :: dx,sum,x, total_sum
      REAL(kind(1.d0)) :: f, mypi, pi
      REAL(kind(1.d0)), parameter :: PI25DT = acos(-1.d0)

      CALL MPI_INIT(ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ierr)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ierr)

      CALL GET_COMMAND_ARGUMENT(1, sintervals)
      READ(sintervals,*)intervals
      intervals = intervals / nproc

      if(myrank == 0) then
         write(*,*) "MPI version with tastks = ", nproc
         write(*,*) "Number of intervals     = ", intervals
      endif

      istart = (intervals) * myrank
      iend   = (intervals) * (myrank + 1)
      mypi = 0.0;
      pi = 0.0;
      do i = istart, iend
          mypi = mypi + (-1.d0)**REAL(i) / (2.d0 * REAL(i) + 1.d0)
      end do
      CALL MPI_Reduce(mypi, pi ,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD,ierr)
      pi = 4.d0 * pi
      CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
      if(myrank == 0) then
         PRINT '(a13,2x,f30.25)',' Computed PI =', pi
         PRINT '(a13,2x,f30.25)',' The True PI =', PI25DT
         PRINT '(a13,2x,f30.25)',' Error        ', PI25DT-pi
      endif

      CALL MPI_FINALIZE(ierr)

      end program

