C --- Main
      program main
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: e,s1,s2,s3,s4,s5,s6
      doubleprecision,dimension(:),allocatable :: sm
      character (len=80) :: com

      nmax=10000

c --- input data
      open(10,file='stress.txt',status='old')
      read(10,100) com
      write(6,100) com
      n=0
      do i=1,nmax
         read(10,*,end=999)
         n=n+1
      enddo
999   continue
      write(6,*) '# of data:',n

      allocate(e(n),s1(n),s2(n),s3(n),s4(n),s5(n),s6(n))
      rewind(10)

      read(10,100) com
      do i=1,n
         read(10,*) e(i),s1(i),s2(i),s3(i),s4(i),s5(i),s6(i)
      enddo
      close(10)
 
c --- check data
c      do i=1,n
c         write(6,*) e(i),s1(i),s2(i),s3(i),s4(i),s5(i),s6(i)
c      enddo

c --- maximun
      smx=abs(s3(1))
      do i=1,n
         s3a=abs(s3(i))
         if(smx.lt.s3a) smx=s3a
      enddo
      write(6,*) 'Max. of s3:',smx

c --- mises stress
      allocate(sm(n))
      do i=1,n
         sm(i)=sqrt(0.5d0*((s1(i)-s2(i))**2+(s2(i)-s3(i))**2
     &      +(s3(i)-s1(i))**2
     &      +6.0d0*(s4(i)**2+s5(i)**2+s6(i)**2)))
      enddo 

c --- maximun
      smx=sm(1)
      do i=1,n
c      do i=1,n,300
         if(smx.lt.sm(i)) smx=sm(i)
      enddo
      write(6,*) 'Max. of mises:',smx

c --- output data
      open(20,file='mises.txt',status='unknown')
      do i=1,n
         write(20,200) e(i),sm(i)
      enddo

100   format(a80)
200   format(2e15.7)

      end
