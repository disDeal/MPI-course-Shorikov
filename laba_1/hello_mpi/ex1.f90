      program ex1 
 
       
      implicit none
      integer :: i, j, k, n
      real :: a, b
      double precision ::c(10,10)
      double precision, allocatable ::m(:,:)
      external rdwr
      
      allocate (m(5,7))

      write(6,*)'input: '
      read(5,*) a
      write(6, '(a,f8.4)')'a = ',a
      write(6,*)'a = ',a
      write(6,100)'a = ',a,a*2.0d0,a*3.0d0,10

      write(6,'(a,f8.4)')'a = ', a


      deallocate (m)
      open(10, file='A',form='formatted',status='unknown')
      read(10,*)k
      read(10,*)n



      allocate (m(k,n+1))
      ! do i = 1,k
      !     read(10,*)(m(i,j),j=1,n)
      !     write(6, '(100f8.4)')(m(i,j),j=1,n)
      !enddo
      call rdwr(10,m,k,n,.true.) 
      call rdwr(6,m,k,n,.false.) 



      deallocate (m)
100   format(a, 3f8.3, i4)


      end
