      program ex2 
      implicit none
      include 'mpif.h'
      integer :: nproc, ierr, rank, len, i, NTIMES
      parameter (NTIMES = 100)
      character* (MPI_MAX_PROCESSOR_NAME) name
      double precision time_start, time_finish, tick


      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_get_processor_name(name, len, ierr)

      tick = MPI_WTICK(ierr)
      time_start = MPI_WTIME(ierr)
      do i = 1,NTIMES
          time_finish = MPI_WTIME(ierr)
      enddo

      write(6,*) 'processor', name(1:len)
      write(6,*) ', process', rank, ': tick = ', tick
      write(6,*) ', time = ', (time_finish - time_start)/NTIMES

      call MPI_FINALIZE(ierr)

      end
