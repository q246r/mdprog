      program main
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: m
      doubleprecision,dimension(:),allocatable :: 
     & a,b,c,d,e,f,st1
      character (len=80) :: com

      nx=100000
      allocate(m(nx),a(nx),b(nx),c(nx),d(nx),e(nx),f(nx),st1(nx))

c --- Read data
      open(10,file='stress.d',status='old')
      read(10,100) com
c      write(6,100) com
      read(10,100) com
c      write(6,100) com
      n=0
      do i=1,nx
       read(10,*,end=99) m(i),a(i),b(i),c(i),d(i),e(i),f(i)
c       write(6,*) m(i),a(i),b(i),c(i),d(i),e(i),f(i)
       n=n+1
      enddo
99    continue
100   format(a80)
      write(6,*) 'n =',n

      do i=1,n
       st1(i)=a(i)
      enddo

      ist=1
      ien=n
      write(6,*) 'ist,ien =',ist,ien
      sum=0.0d0
      do i=ist,ien
       sum=sum+st1(i)
      enddo
      nd=ien-ist+1
      write(6,*) 'nd =',nd
      st1ave=sum/nd
      write(6,*) 'st1ave (GPa) =',st1ave*1.0d-4
 
      ist=n*0.8+1
      ien=n
      write(6,*) 'ist,ien =',ist,ien
      sum=0.0d0
      do i=ist,ien
       sum=sum+st1(i)
      enddo
      nd=ien-ist+1
      write(6,*) 'nd =',nd
      st1ave=sum/nd
      write(6,*) 'st1ave (GPa) =',st1ave*1.0d-4 
 
      end
