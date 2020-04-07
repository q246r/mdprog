c
c Program to obtain hydrogen concentration
c
      module variables
       integer,dimension(:),allocatable :: is
       doubleprecision,dimension(:),allocatable ::
     &  x,y,z
       integer :: n,nty
       doubleprecision :: xmax,xmin,ymax,ymin,zmax,zmin
      end
c
c --- Main program
      program main
      use variables
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: xd(:),yd(:),hc(:)

      nmax=max(n,1000000)
      allocate(is(nmax),x(nmax),y(nmax),z(nmax))
      ndmax=10000
      allocate(xd(ndmax),yd(ndmax),hc(ndmax))

      idis=0

c --- Read dump file
      call readdump
c --- Check data
c      do i=1,10
c       write(6,*) is(i),x(i),y(i),z(i)
c      enddo

c --- Make coordinate data
      nd=1
      ndx=4
      ndy=4
      dx=(xmax-xmin)/ndx
      dy=(ymax-ymin)/ndy
      do i=1,ndx
      do j=1,ndy
       xd(nd)=xmin+(i-1)*dx
       yd(nd)=ymin+(j-1)*dy
       nd=nd+1
      enddo
      enddo
      nd=nd-1
      if(idis.eq.1) write(6,*) 'nd =',nd

c --- Calc. H concentration
      rc=5.0d0
      z0=0.0d0

      do j=1,nd

      ih=0
      ipd=0
      nc=0
      x0=xd(j)
      y0=yd(j)
      x1=x0+rc
      y1=y0+rc
      z1=z0+rc
      do i=1,n
       if(x(i).gt.x0.and.x(i).lt.x1) then
       else if(y(i).gt.y0.and.y(i).lt.y1) then
       else if(z(i).gt.z0.and.z(i).lt.z1) then
        if(is(i).eq.1) ipd=ipd+1
        if(is(i).eq.2) ih=ih+1
        nc=nc+1
c       endif
c       endif
       endif
      enddo
c      write(6,*) 'xmin,ymin,zmin =',xmin,ymin,zmin
c      write(6,*) 'xmax,ymax,zmax =',xmax,ymax,zmax
c      write(6,*) 'x0,y0,z0 =',x0,y0,z0
c      write(6,*) 'x1,y1,z1 =',x1,y1,z1
      if(idis.eq.1) write(6,*) 'ih,ipd,nc =',ih,ipd,nc
      if(idis.eq.1) write(6,*) 'hconc =',real(ih)/real(ipd)
      hc(j)=real(ih)/real(ipd)

      enddo

c --- Write data
      do i=1,nd
       write(6,100) xd(i),yd(i),hc(i)
      enddo
100   format(3e15.7)

      end
c
c --- dump style = atom
      subroutine readdump
      use variables
      implicit doubleprecision (a-h,o-z)
      character (len=80) :: com

      open(10,file='../test/res0',status='old')

      read(10,100) com
      if(idis.eq.1) write(6,100) com
      read(10,100) com
      if(idis.eq.1) write(6,100) com
      read(10,100) com
      if(idis.eq.1) write(6,100) com
      read(10,*) n
      if(idis.eq.1) write(6,*) 'n =',n
      read(10,100) com
      if(idis.eq.1) write(6,100) com
      read(10,*) xmin,xmax
      if(idis.eq.1) write(6,*) 'xmin, xmax =',xmin,xmax
      read(10,*) ymin,ymax
      if(idis.eq.1) write(6,*) 'ymin, ymax =',ymin,ymax
      read(10,*) zmin,zmax
      if(idis.eq.1) write(6,*) 'zmin, zmax =',zmin,zmax
      read(10,100) com
      if(idis.eq.1) write(6,100) com
      do i=1,n
       read(10,*) m,is(i),x(i),y(i),z(i)
      enddo

      close(10)
100   format(a80)

      xl=xmax-xmin
      yl=ymax-ymin
      zl=zmax-zmin
      do i=1,n
       x(i)=x(i)*xl
       y(i)=y(i)*yl
       z(i)=z(i)*zl
      enddo

      end

