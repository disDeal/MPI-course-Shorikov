      program ex3 
      implicit none
      include 'mpif.h'
      integer :: nproc, rank, len, a, b, ierr
      integer status(MPI_STATUS_SIZE)
      character* (MPI_MAX_PROCESSOR_NAME) name

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_get_processor_name(name, len, ierr)

      a = 0
      b = 0
      write(6, *)'a:', a, 'b:', b
      if (rank.eq.0) then
          write(6, *)'input: '
          read(5, *)a
          write(6, *)'<sender> input is ',a
      endif

      if (rank.eq.0) then
          call MPI_SEND(a, 1, MPI_INTEGER, nproc-1, 5, MPI_COMM_WORLD, ierr)
      elseif (rank.eq.(nproc-1)) then
          write(6, *)'<resiever> b before ',b
          call MPI_RECV(b, 1, MPI_INTEGER, 0, 5, MPI_COMM_WORLD, status, ierr)
          write(6, *)'<resiever> b after ', b
      endif

      call MPI_FINALIZE(ierr)

      end
