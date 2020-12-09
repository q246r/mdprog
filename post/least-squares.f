c
c least squares method
c
      module data
       integer :: ntot
       doubleprecision,dimension(:),allocatable :: x,y
       doubleprecision :: a,b
      end
c
      program main
      use data
      implicit doubleprecision (a-h,o-z)

      nd=100
      allocate(x(nd),y(nd))

      write(6,*) 'Reading data file ...'
      open(10,file='s22e.txt',status='old')
      ntot=0
      do i=1,nd
       read(10,*,end=99) x(i),y(i)
       write(6,*) x(i),y(i)
       ntot=ntot+1
      enddo
99    continue
      close(10)
      write(6,*) '# of data:',ntot

      call least_squares
      write(6,*) 'least_squares has been finished.'

      write(6,*) 'a =',a
      write(6,*) 'b =',b

      end
c
      subroutine least_squares
      use data

      write(6,*) 'least_squares is started.'
c      do i=1,ntot
c       write(6,*) i,x(i),y(i)
c      enddo

      xsum=0.0d0
      ysum=0.0d0
      do i=1,ntot
       xsum=xsum+x(i)
       ysum=ysum+y(i)
      enddo
      xave=xsum/ntot
      yave=ysum/ntot
      write(6,*) 'ntot =',ntot
      write(6,*) 'xave:',xave,'yave:',yave

      sxy=0.0d0
      sx2=0.0d0
      do i=1,ntot
       sxy=(x(i)-xave)*(y(i)-yave)
       sx2=(x(i)-xave)*(x(i)-xave)
      enddo
      a=sxy/sx2
      b=yave-a*xave
      write(6,*) 'a:',a,'b:',b

      end
