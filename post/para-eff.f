      program main
      integer,dimension(:),allocatable :: np
      real,dimension(:),allocatable :: et,ef

      nx=100
      allocate(np(nx),et(nx),ef(nx))

      do i=1,nx
       np(i)=0
       et(i)=0.0
      enddo

c --- Input data
      open(10,file='result',status='old')
      i=1
      do
       read(10,*,end=100) np(i),et(i)
       i=i+1
      enddo
100   close(10)
      n=i-1
      write(6,*) '# of data =',n
      do i=1,n
       write(6,10000) np(i),et(i)
      enddo

c --- Strong scaling
      do i=1,n
       ef(i)=et(1)/et(i)/real(np(i))
      enddo
c --- Weak scaling
c      do i=1,n
c       ef(i)=et(1)/et(i)
c      enddo

c --- Output data
      write(6,*) 'Parallel efficiency:'
      open(11,file='efficiency',status='unknown')
      do i=1,n
       write(11,10000) np(i),ef(i)
       write(6,10000) np(i),ef(i)
      enddo

10000 format(i6,e15.7)
      end
