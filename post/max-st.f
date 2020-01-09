      program main
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: m
      doubleprecision,dimension(:),allocatable :: 
     & a,b,c,d,e,f,st1
      character (len=80) :: com

      nx=100000
      allocate(m(nx),a(nx),b(nx),c(nx),d(nx),e(nx),f(nx),st1(nx))

      dt=1.0d-8
c      dt=1.0d0
      b2g=1.0d-4

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
      close(10)
100   format(a80)
      write(6,*) 'n =',n

      do i=1,n
       a(i)=-a(i)
       b(i)=-b(i)
       c(i)=-c(i)
       d(i)=-d(i)
       e(i)=-e(i)
       f(i)=-f(i)
      enddo

c --- Max stress
      smax=a(1)
      mmax=m(1)
      do i=1,n
       if(a(i).gt.smax) then
        smax=a(i)
        mmax=m(i)
       endif
      enddo
      write(6,*) 'st1 max (GPa), strain:'
      write(6,*) smax*b2g,mmax*dt

c --- Write data
      open(20,file='stress1.d',status='unknown')
      write(20,*) '# strain,st1,st2,st3,st4,st5,st6'
      do i=1,n
       write(20,200) m(i)*dt,a(i)*b2g,b(i)*b2g,c(i)*b2g,d(i)*b2g,
     &  e(i)*b2g,f(i)*b2g
      enddo
      close(20)
200   format(7e15.7)
 
      end
