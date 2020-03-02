 
      subroutine rdwr(unt,a,n,m,sw)
      implicit none
      integer :: n,m,unt
      double precision ::a(n,m)
      logical :: sw
      integer :: i, j

      if (sw) then
          do i = 1,n
              read(unt,*)(a(i,j),j=1,m)
          enddo
      else
          do i = 1,n
              write(unt,'(100f8.4)')(a(i,j),j=1,m)
          enddo
      endif

      return
      end 
