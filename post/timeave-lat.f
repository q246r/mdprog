      program main
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: m
      doubleprecision,dimension(:),allocatable :: 
     & a,b,c,d,vol
      character (len=80) :: com

      nx=100000
      allocate(m(nx),a(nx),b(nx),c(nx),d(nx),vol(nx))

c --- Read data
      open(10,file='thermo.d',status='old')
      read(10,100) com
c      write(6,100) com
      n=0
      do i=1,nx
       read(10,*,end=99) m(i),a(i),b(i),c(i),d(i)
       n=n+1
      enddo
99    continue
100   format(a80)
      write(6,*) 'n =',n

      do i=1,n
       vol(i)=c(i)
      enddo

      ist=1
      ien=n
      write(6,*) 'ist,ien =',ist,ien
      sum=0.0d0
      do i=ist,ien
       sum=sum+vol(i)
      enddo
      nd=ien-ist+1
      write(6,*) 'nd =',nd
      vave=sum/nd
      write(6,*) 'vave (A^3) =',vave 
      a0=vave**(1.0d0/3.0d0)/10.0d0
      write(6,*) 'a0 (A) =',a0
 
      ist=n*0.8+1
      ien=n
      write(6,*) 'ist,ien =',ist,ien
      sum=0.0d0
      do i=ist,ien
       sum=sum+vol(i)
      enddo
      nd=ien-ist+1
      write(6,*) 'nd =',nd
      vave=sum/nd
      write(6,*) 'vave (A^3) =',vave 
      a0=vave**(1.0d0/3.0d0)/10.0d0
      write(6,*) 'a0 (A) =',a0
 
      end
