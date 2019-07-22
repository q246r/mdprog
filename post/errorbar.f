c Error bar
c  Input file:  x, y1, y2, y3 ...
c  Output file: x, y_ave, y_min, y_max
      program main
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: 
     & x,yave,ymin,ymax,s
      doubleprecision,dimension(:,:),allocatable :: y
      character (len=80) :: com

      nx=100
      allocate(x(nx),yave(nx),ymin(nx),ymax(nx),s(nx))
      ny=100
      allocate(y(nx,ny))
      nd=3

c --- Read data
      open(10,file='input.dat',status='old')
      read(10,100) com
      write(6,100) com
      n=0
      do i=1,nx
       read(10,*,end=99) x(i),y(i,1),y(i,2),y(i,3)
c       write(6,*) x(i),y(i,1),y(i,2),y(i,3)
       n=n+1
      enddo
99    continue
100   format(a80)
      write(6,*) 'n =',n

      do i=1,n
       s(i)=0.0d0
      enddo
      do i=1,n
      ymin(i)=y(i,1)
      ymax(i)=y(i,1)
      do j=1,nd 
       s(i)=s(i)+y(i,j)
       if(y(i,j).lt.ymin(i)) ymin(i)=y(i,j)
       if(y(i,j).ge.ymax(i)) ymax(i)=y(i,j)
      enddo
      enddo
      do i=1,n
       yave(i)=s(i)/nd
      enddo
c --- Check
c      do i=1,n
c       write(6,*) x(i),s(i)/nd,ymin(i),ymax(i)
c      enddo

c --- Write data
      open(20,file='output.dat',status='unknown')
      write(20,200)
200   format('# x, yave, ymin, ymax')
      do i=1,n
       write(20,300) x(i),yave(i),ymin(i),ymax(i)
       write(6,300) x(i),yave(i),ymin(i),ymax(i)
      enddo
300   format(4e15.7)

      end
