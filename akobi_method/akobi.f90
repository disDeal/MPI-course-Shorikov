
      program akobi
      implicit none

      include "mpif.h"
      INTEGER :: ncol, nrow, i, j, ierr
      INTEGER :: rank, nproc
      REAL, ALLOCATABLE :: A(:,:), B(:,:), D(:,:), D_rev(:,:), E(:,:)
      REAL, ALLOCATABLE :: x(:), x_prev(:)
      INTEGER status(MPI_STATUS_SIZE)

      CALL MPI_INIT(ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

      if (rank .eq. 0) then
          open(10, file='B', form='formatted', status='unknown')
          read(10, *)ncol
          read(10, *)nrow

          ALLOCATE (A(ncol, nrow))
          do i = 1,ncol
              read(10,*)(A(i,j),j=1,nrow)
          enddo

          write(6,*) "ncol:", ncol, ", nrow:", nrow
          write(6,*) "matrix:"
          do i = 1,ncol
              write(6,101)(A(i,j),j=1,nrow)
          enddo

          ALLOCATE(x(ncol))
          open(10, file='x', form='formatted', status='unknown')
          do i = 1,ncol
              read(10,*)x(i)
          enddo

          do i = 1,ncol
              write(6,101)x(i)
          enddo

          ALLOCATE (B(ncol, nrow))
          ALLOCATE (D(ncol, nrow))
          ALLOCATE (D_rev(ncol, nrow))
          ALLOCATE (E(ncol, nrow))

          do i = 1,ncol
              D(i, i) = A(i, i)
              D_rev(i,i) = 1 / A(i, i)
              E(i, i) = 1
          enddo
          do i = 1,ncol
              write(6,101)(D(i,j),j=1,nrow)
          enddo


       endif


      CALL MPI_FINALIZE(ierr)

100   format(100i4)
101   format(6f8.3)
      end
