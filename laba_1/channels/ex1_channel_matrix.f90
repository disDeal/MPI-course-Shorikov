      program ex3 
      implicit none
      include 'mpif.h'
      integer :: nproc, rank, len, i, j, ierr
      integer :: col, row
      REAL, allocatable :: arr(:,:)
      REAL, allocatable  :: new(:,:)
      integer status(MPI_STATUS_SIZE)
      character* (MPI_MAX_PROCESSOR_NAME) name

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      call MPI_get_processor_name(name, len, ierr)

      open(10, file='A', form='formatted', status='unknown')
      read(10, *)col
      read(10, *)row

      ALLOCATE (arr(col, row))
      do i = 1,col
          read(10,*)(arr(i,j),j=1,row)
      enddo

      if (rank.eq.0) then
          call MPI_SEND(col, 1, MPI_INTEGER, nproc-1, 1, MPI_COMM_WORLD, ierr)
          call MPI_SEND(row, 1, MPI_INTEGER, nproc-1, 2, MPI_COMM_WORLD, ierr)
          call MPI_SEND(arr, col*row, MPI_REAL, nproc-1, 5, MPI_COMM_WORLD, ierr)
      elseif (rank.eq.(nproc-1)) then
          call MPI_RECV(col, 1, MPI_INTEGER, 0, 1, MPI_COMM_WORLD, status, ierr)
          call MPI_RECV(row, 1, MPI_INTEGER, 0, 2, MPI_COMM_WORLD, status, ierr)

          ALLOCATE (new(col, row))
          call MPI_RECV(new, col*row, MPI_REAL, 0, 5, MPI_COMM_WORLD, status, ierr)

          do i = 1,col
              write(6,*)(new(i,j),j=1,row)
          enddo

      endif

      call MPI_FINALIZE(ierr)

      end
