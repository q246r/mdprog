c
c Program for initial atom arrengement
c
      module variables
       integer,dimension(:),allocatable :: is
       doubleprecision,dimension(:),allocatable ::
     &  x,y,z
       integer :: n,nty
       doubleprecision :: xmax,xmin,ymax,ymin,zmax,zmin
      end
c
      module parameters
       integer :: idis,ndis,nrot,js,nb
       doubleprecision :: x0,x1,y0,y1,z0,z1,dw,xv,yv,zv
       integer :: nx,ny,nz,nh,mx,my,mz,nsub
       doubleprecision :: ac,rc,cc
       doubleprecision,dimension(3) :: px,py,pz
       doubleprecision,dimension(:),allocatable :: rx,ry,rz
      end
c
c --- Main program
      program main
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Display mode: 0=Run, 1=Debug
      idis=1

      if(idis.eq.1) write(6,*) 'Program modeler1 is started.'

c --- Set parameters
c      call setpara
c      if(idis.eq.1) write(6,*) 'setpara has been finished.'
      call setpara2
      if(idis.eq.1) write(6,*) 'setpara2 has been finished.'

c --- Select job
      if(js.lt.0) then
       write(6,*) 'Could you set job selecter (js).'
       stop
      endif
      if(js.eq. 1) call fcc100
      if(js.eq. 2) call fcc111
      if(js.eq. 3) call fcc0
      if(js.eq. 4) call dia0
      if(js.eq. 5) call zb0
      if(js.eq. 6) call rs0
      if(js.eq. 7) call fcc3c
      if(js.eq. 8) call dia3c
      if(js.eq. 9) call zb3c
      if(js.eq.11) call diamond
      if(js.eq.12) call zincblende
c      if(js.eq.13) call sic2h
      if(js.eq.14) call sic4h
c      if(js.eq.15) call sic6h
      if(js.eq.16) call fcc111r
      if(js.eq.21) call fcc111e
      if(js.eq.23) call fcc111e2
      if(js.eq.27) call fcc111ef
      if(js.eq.31) call fcc111sc
      if(js.eq.32) call fcc111scqd
      if(js.eq.51) call fcc100v
      if(js.eq.52) call fcc0alloy
      if(js.eq.61) call fcch
      if(js.eq.62) call fcc100h
      if(js.eq.63) call fcc111h
      if(js.eq.64) call rs100vac
      if(js.eq.65) call rs100vacr
      if(js.eq.66) call rs100intsub
      if(js.eq.71) call indent
      if(js.eq.72) call indentzb
      if(js.eq.73) call indentzbz
      if(js.eq.74) call indentz
      if(js.eq.75) call indentzh
      if(js.eq.76) call indentrsz
      if(js.eq.77) call indentrsvacz
      if(js.eq.81) call sicwire
      if(js.eq.82) call fcc100wire
      if(js.eq.83) call cuwire
      if(js.eq.99) call dump2data
      if(idis.eq.1) write(6,*) 
     & 'Generation of atom arrangement has been finished.'

c --- Output data
c --- LAMMPS data file
      call writedata
      if(idis.eq.1) write(6,*) 'writedata has been finished.'

      if(idis.eq.1) write(6,*) 'Program modeler1 has been finished.'

      end
c
      subroutine setpara
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'setpara is started.' 

c --- Number of hydrogen
c      nh=0
      nh=512
c --- Number of lattice
c --- For test
c      nx=3
c      ny=3
c      nz=3
c      nx=10
c      ny=10
c      nz=10
c --- For large system
c ---  {100} stacking
c      nx=20
c      ny=20
c      nz=20
c ---  {111} stacking
c      nx=16
c      ny=12
c      nz=28
c --- For screw dislocation
      nx=18
      ny=13
      nz=30
c      nx=9
c      ny=6
c      nz=15

c --- Lattice constant
      fac=1.0d0
      ac=3.885d0*fac

c --- Cut-off distance
      rc=5.35d0
c --- Number of atom type
      nty=1

c --- Select dislocation
c --- 0: without dislocation
c --- 1: screw dislocation
c --- 2: edge dislocation
      ndis=1

c --- Select rotation
c --- 0: without rotation
c --- 1: rotate
      nrot=1

      end
c
      subroutine setpara2
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      character (len=80) :: com

      if(idis.eq.1) write(6,*) 'setpara2 is started.' 

c --- 10 integer parameters and 10 real parameters are input.

c --- Comment
      read(5,100) com
      write(6,200) com
c --- Job selecter
      read(5,100) com
      read(5,*) js
c --- Lattice, nx
      read(5,100) com
      read(5,*) nx
c --- Lattice, ny
      read(5,100) com
      read(5,*) ny
c --- Lattice, nz
      read(5,100) com
      read(5,*) nz
c --- Number of H
      read(5,100) com
      read(5,*) nh
c --- Number of atom type
      read(5,100) com
      read(5,*) nty
c --- MD box, mx
      read(5,100) com
      read(5,*) mx
c --- MD box, my
      read(5,100) com
      read(5,*) my
c --- MD box, mz
      read(5,100) com
      read(5,*) mz
c --- Dumy 10
      read(5,100) com
      read(5,*) nsub

c --- Lattice constant, a
      read(5,100) com
      read(5,*) ac
c --- Cut-off distance
      read(5,100) com
      read(5,*) rc
c --- Lattice constant, c
      read(5,100) com
      read(5,*) cc
c --- Dumy 4
      read(5,100) com
      read(5,*) dumy
c --- Dumy 5
      read(5,100) com
      read(5,*) dumy
c --- Dumy 6
      read(5,100) com
      read(5,*) dumy
c --- Dumy 7
      read(5,100) com
      read(5,*) dumy
c --- Dumy 8
      read(5,100) com
      read(5,*) dumy
c --- Dumy 9
      read(5,100) com
      read(5,*) dumy
c --- Dumy 10
      read(5,100) com
      read(5,*) dumy

c --- Set array
      nn=nx*ny*nz*10
      nn=max(nn,1000)
      allocate(x(nn),y(nn),z(nn),is(nn))

c --- Initialization of array
      do i=1,nn
       x(i)=0.0d0
       y(i)=0.0d0
       z(i)=0.0d0
       is(i)=1
      enddo

100   format(a80)
200   format(1x,a80)

      end
c
      subroutine fcc100
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc100 is started.'

      nd=1
      do i=1,nx
      do j=1,ny
      do k=1,nz
       x(nd)=real(i-1)
       y(nd)=real(j-1)
       z(nd)=real(k-1)

       x(nd+1)=real(i-1)+0.5d0
       y(nd+1)=real(j-1)+0.5d0
       z(nd+1)=real(k-1)

       x(nd+2)=real(i-1)
       y(nd+2)=real(j-1)+0.5d0
       z(nd+2)=real(k-1)+0.5d0

       x(nd+3)=real(i-1)+0.5d0
       y(nd+3)=real(j-1)
       z(nd+3)=real(k-1)+0.5d0

       nd=nd+4
      enddo
      enddo
      enddo
      n=nd-1

c --- Make IS
      do i=1,n
       is(i)=1
      enddo

c --- Angstrom, ac: lattice constant
      do i=1,n
       x(i)=x(i)*ac
       y(i)=y(i)*ac
       z(i)=z(i)*ac
      enddo

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

      end
c
      subroutine fcc0
c
c Primitive vector: px, py, pz
c Relative coordinate: rx, ry, rz
c Coordinate: x, y, z
c
c  x = px(1)*rx + px(2)*ry + px(3)*rz
c  y = py(1)*rx + py(2)*ry + py(3)*rz
c  z = pz(1)*rx + pz(2)*ry + pz(3)*rz
c
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc0 is started.'

      nb=4
      allocate(rx(nb),ry(nb),rz(nb))

c Primitive vector
      px(1)=ac 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=0.0d0 
      py(2)=ac 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=ac 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=0.5d0
      ry(2)=0.5d0
      rz(2)=0.0d0
      is(2)=1

      rx(3)=0.0d0
      ry(3)=0.5d0
      rz(3)=0.5d0
      is(3)=1

      rx(4)=0.5d0
      ry(4)=0.0d0
      rz(4)=0.5d0
      is(3)=1

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

      end
c
      subroutine dia0
c
c Primitive vector: px, py, pz
c Relative coordinate: rx, ry, rz
c Coordinate: x, y, z
c
c  x = px(1)*rx + px(2)*ry + px(3)*rz
c  y = py(1)*rx + py(2)*ry + py(3)*rz
c  z = pz(1)*rx + pz(2)*ry + pz(3)*rz
c
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'dia0 is started.'

      nb=8
      allocate(rx(nb),ry(nb),rz(nb))

c Primitive vector
      px(1)=ac 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=0.0d0 
      py(2)=ac 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=ac 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=0.5d0
      ry(2)=0.5d0
      rz(2)=0.0d0
      is(2)=1

      rx(3)=0.0d0
      ry(3)=0.5d0
      rz(3)=0.5d0
      is(3)=1

      rx(4)=0.5d0
      ry(4)=0.0d0
      rz(4)=0.5d0
      is(4)=1

      rx(5)=0.25d0
      ry(5)=0.25d0
      rz(5)=0.25d0
      is(5)=1

      rx(6)=0.25d0
      ry(6)=0.75d0
      rz(6)=0.75d0
      is(6)=1

      rx(7)=0.75d0
      ry(7)=0.25d0
      rz(7)=0.75d0
      is(7)=1

      rx(8)=0.75d0
      ry(8)=0.75d0
      rz(8)=0.25d0
      is(8)=1

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

      end
c
      subroutine zb0
c
c Primitive vector: px, py, pz
c Relative coordinate: rx, ry, rz
c Coordinate: x, y, z
c
c  x = px(1)*rx + px(2)*ry + px(3)*rz
c  y = py(1)*rx + py(2)*ry + py(3)*rz
c  z = pz(1)*rx + pz(2)*ry + pz(3)*rz
c
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'zb0 is started.'

      nb=8
      allocate(rx(nb),ry(nb),rz(nb))

c Primitive vector
      px(1)=ac 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=0.0d0 
      py(2)=ac 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=ac 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=0.5d0
      ry(2)=0.5d0
      rz(2)=0.0d0
      is(2)=1

      rx(3)=0.0d0
      ry(3)=0.5d0
      rz(3)=0.5d0
      is(3)=1

      rx(4)=0.5d0
      ry(4)=0.0d0
      rz(4)=0.5d0
      is(4)=1

      rx(5)=0.25d0
      ry(5)=0.25d0
      rz(5)=0.25d0
      is(5)=2

      rx(6)=0.25d0
      ry(6)=0.75d0
      rz(6)=0.75d0
      is(6)=2

      rx(7)=0.75d0
      ry(7)=0.25d0
      rz(7)=0.75d0
      is(7)=2

      rx(8)=0.75d0
      ry(8)=0.75d0
      rz(8)=0.25d0
      is(8)=2

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

      end
c
      subroutine rs0
c
c Primitive vector: px, py, pz
c Relative coordinate: rx, ry, rz
c Coordinate: x, y, z
c
c  x = px(1)*rx + px(2)*ry + px(3)*rz
c  y = py(1)*rx + py(2)*ry + py(3)*rz
c  z = pz(1)*rx + pz(2)*ry + pz(3)*rz
c
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'rs0 is started.'

      nb=8
      allocate(rx(nb),ry(nb),rz(nb))

c Primitive vector
      px(1)=ac 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=0.0d0 
      py(2)=ac 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=ac 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=0.5d0
      ry(2)=0.5d0
      rz(2)=0.0d0
      is(2)=1

      rx(3)=0.0d0
      ry(3)=0.5d0
      rz(3)=0.5d0
      is(3)=1

      rx(4)=0.5d0
      ry(4)=0.0d0
      rz(4)=0.5d0
      is(4)=1

      rx(5)=0.0d0
      ry(5)=0.0d0
      rz(5)=0.5d0
      is(5)=2

      rx(6)=0.0d0
      ry(6)=0.5d0
      rz(6)=0.0d0
      is(6)=2

      rx(7)=0.5d0
      ry(7)=0.0d0
      rz(7)=0.0d0
      is(7)=2

      rx(8)=0.5d0
      ry(8)=0.5d0
      rz(8)=0.5d0
      is(8)=2

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

      end
c
      subroutine fcc3c
c
c Primitive vector: px, py, pz
c Relative coordinate: rx, ry, rz
c Coordinate: x, y, z
c
c  x = px(1)*rx + px(2)*ry + px(3)*rz
c  y = py(1)*rx + py(2)*ry + py(3)*rz
c  z = pz(1)*rx + pz(2)*ry + pz(3)*rz
c
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc3c is started.'

      nb=3
      allocate(rx(nb),ry(nb),rz(nb))

      r13=1.0d0/3.0d0
      r23=2.0d0/3.0d0
      r2=sqrt(2.0d0)
      r3=sqrt(3.0d0)

      a=ac*r2
      c=a*2.0d0*r2/r3

      if(idis.eq.1) write(6,*) 'ac =',ac
      if(idis.eq.1) write(6,*) 'a  =',a
      if(idis.eq.1) write(6,*) 'c  =',c

c Primitive vector
      px(1)=a 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=-a*0.5d0 
      py(2)=a*r3*0.5d0 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=c 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=r13
      ry(2)=r23
      rz(2)=r13
      is(2)=1

      rx(3)=r23
      ry(3)=r13
      rz(3)=r23
      is(3)=1

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      dx=-1.0d-6
      dy=0.0d0
      dz=0.0d0
      xmin=0.0d0+dx
      xmax=mx*a+dx
      ymin=0.0d0+dy
      ymax=my*a*r3+dy
      zmin=0.0d0+dz
      zmax=mz*c+dz

c --- Cut out MD box 
      call cutbox
      if(idis.eq.1) write(6,*) 'cutbox has been finished.'

c --- Check
c      write(6,*) 'xmin,xmax:',xmin,xmax
c      write(6,*) 'ymin,ymax:',ymin,ymax
c      write(6,*) 'zmin,zmax:',zmin,zmax
c      do i=1,n
c       write(6,100) i,x(i),y(i),z(i)
c      enddo
c100   format(i6,3e15.7)

      end
c
      subroutine dia3c
c
c Primitive vector: px, py, pz
c Relative coordinate: rx, ry, rz
c Coordinate: x, y, z
c
c  x = px(1)*rx + px(2)*ry + px(3)*rz
c  y = py(1)*rx + py(2)*ry + py(3)*rz
c  z = pz(1)*rx + pz(2)*ry + pz(3)*rz
c
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'dia3c is started.'

      nb=6
      allocate(rx(nb),ry(nb),rz(nb))

      r13=1.0d0/3.0d0
      r23=2.0d0/3.0d0
      r2=sqrt(2.0d0)
      r3=sqrt(3.0d0)

      a=ac*r2*0.5d0
      c=ac*r3
      u=0.25d0

      if(idis.eq.1) write(6,*) 'ac =',ac
      if(idis.eq.1) write(6,*) 'a  =',a
      if(idis.eq.1) write(6,*) 'c  =',c
      if(idis.eq.1) write(6,*) 'u  =',u

c Primitive vector
      px(1)=a 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=-a*0.5d0 
      py(2)=a*r3*0.5d0 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=c 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=r13
      ry(2)=r23
      rz(2)=r13
      is(2)=1

      rx(3)=r23
      ry(3)=r13
      rz(3)=r23
      is(3)=1

      rx(4)=0.0d0
      ry(4)=0.0d0
      rz(4)=0.0d0+u
      is(4)=1

      rx(5)=r13
      ry(5)=r23
      rz(5)=r13+u
      is(5)=1

      rx(6)=r23
      ry(6)=r13
      rz(6)=r23+u
      is(6)=1

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      dx=-1.0d-6
      dy=0.0d0
      dz=0.0d0
      xmin=0.0d0+dx
      xmax=mx*a+dx
      ymin=0.0d0+dy
      ymax=my*a*r3+dy
      zmin=0.0d0+dz
      zmax=mz*c+dz

c --- Cut out MD box 
      call cutbox
      if(idis.eq.1) write(6,*) 'cutbox has been finished.'

c --- Check
c      write(6,*) 'xmin,xmax:',xmin,xmax
c      write(6,*) 'ymin,ymax:',ymin,ymax
c      write(6,*) 'zmin,zmax:',zmin,zmax
c      do i=1,n
c       write(6,100) i,x(i),y(i),z(i)
c      enddo
c100   format(i6,3e15.7)

      end
c
      subroutine zb3c
c
c Primitive vector: px, py, pz
c Relative coordinate: rx, ry, rz
c Coordinate: x, y, z
c
c  x = px(1)*rx + px(2)*ry + px(3)*rz
c  y = py(1)*rx + py(2)*ry + py(3)*rz
c  z = pz(1)*rx + pz(2)*ry + pz(3)*rz
c
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'zb3c is started.'

      nb=6
      allocate(rx(nb),ry(nb),rz(nb))

      r13=1.0d0/3.0d0
      r23=2.0d0/3.0d0
      r2=sqrt(2.0d0)
      r3=sqrt(3.0d0)

      a=ac*r2*0.5d0
      c=ac*r3
      u=0.25d0

      if(idis.eq.1) write(6,*) 'ac =',ac
      if(idis.eq.1) write(6,*) 'a  =',a
      if(idis.eq.1) write(6,*) 'c  =',c
      if(idis.eq.1) write(6,*) 'u  =',u

c Primitive vector
      px(1)=a 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=-a*0.5d0 
      py(2)=a*r3*0.5d0 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=c 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=r13
      ry(2)=r23
      rz(2)=r13
      is(2)=1

      rx(3)=r23
      ry(3)=r13
      rz(3)=r23
      is(3)=1

      rx(4)=0.0d0
      ry(4)=0.0d0
      rz(4)=0.0d0+u
      is(4)=2

      rx(5)=r13
      ry(5)=r23
      rz(5)=r13+u
      is(5)=2

      rx(6)=r23
      ry(6)=r13
      rz(6)=r23+u
      is(6)=2

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      dx=-1.0d-6
      dy=0.0d0
      dz=0.0d0
      xmin=0.0d0+dx
      xmax=nx*a+dx
      ymin=0.0d0+dy
      ymax=ny*a*r3*0.5d0+dy
      zmin=0.0d0+dz
      zmax=nz*c+dz
      xlen=xmax-xmin
      ylen=ymax-ymin
      zlen=zmax-zmin

      do i=1,n
       if(x(i).gt.xmax) x(i)=x(i)-xlen
       if(x(i).lt.xmin) x(i)=x(i)+xlen
       if(y(i).gt.ymax) y(i)=y(i)-ylen
       if(y(i).lt.ymin) y(i)=y(i)+ylen
       if(z(i).gt.zmax) z(i)=z(i)-zlen
       if(z(i).lt.zmin) z(i)=z(i)+zlen
      enddo

c --- Cut out MD box 
c      call cutbox
c      if(idis.eq.1) write(6,*) 'cutbox has been finished.'

c --- Check
c      write(6,*) 'xmin,xmax:',xmin,xmax
c      write(6,*) 'ymin,ymax:',ymin,ymax
c      write(6,*) 'zmin,zmax:',zmin,zmax
c      do i=1,n
c       write(6,100) i,x(i),y(i),z(i)
c      enddo
c100   format(i6,3e15.7)

      end
c
      subroutine mkxyz
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'mkxyz is started.'

      do i= 1,nb
       x(i)=px(1)*rx(i)+px(2)*ry(i)+px(3)*rz(i) 
       y(i)=py(1)*rx(i)+py(2)*ry(i)+py(3)*rz(i) 
       z(i)=pz(1)*rx(i)+pz(2)*ry(i)+pz(3)*rz(i) 
      enddo
      n=nb

      nd=0
      do i=1,nx
      do j=1,ny
      do k=1,nz
      do m=1,nb
       dx=px(1)*(i-1)+px(2)*(j-1)+px(3)*(k-1)
       dy=py(1)*(i-1)+py(2)*(j-1)+py(3)*(k-1)
       dz=pz(1)*(i-1)+pz(2)*(j-1)+pz(3)*(k-1)
       x(m+nd)=x(m)+dx
       y(m+nd)=y(m)+dy
       z(m+nd)=z(m)+dz
       is(m+nd)=is(m)
c       write(6,*) m,nd,x(m+nd),y(m+nd),z(m+nd)
      enddo
      nd=nd+nb
      enddo
      enddo
      enddo
      n=nd
      if(idis.eq.1) write(6,*) 'n =',n

      end
c
      subroutine cutbox
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: iw
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'cutbox is started.'

c --- Check
c      write(6,*) 'n =',n
c      write(6,*) 'xmin,xmax:',xmin,xmax
c      write(6,*) 'ymin,ymax:',ymin,ymax
c      write(6,*) 'zmin,zmax:',zmin,zmax
c      do i=1,n
c       write(6,100) i,x(i),y(i),z(i)
c      enddo
c100   format(i6,3e15.7)

      nn=nx*ny*nz*10
      nn=max(nn,1000)
      allocate(iw(nn),wx(nn),wy(nn),wz(nn))

      do i=1,n
       iw(i)=0
       wx(i)=0.0d0
       wy(i)=0.0d0
       wz(i)=0.0d0
      enddo

      nd=1
      do i=1,n
       if(x(i).ge.xmin.and.x(i).lt.xmax) then
       if(y(i).ge.ymin.and.y(i).lt.ymax) then
       if(z(i).ge.zmin.and.z(i).lt.zmax) then
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        iw(nd)=is(i)
        nd=nd+1
       endif
       endif
       endif
      enddo
      n=nd-1
c      if(idis.eq.1) write(6,*) 'n =',n

      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=iw(i)
      enddo

      end
c
      subroutine diamond
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'diamond is started.'

      nd=1
      do i=1,nx
      do j=1,ny
      do k=1,nz
       x(nd)=real(i-1)
       y(nd)=real(j-1)
       z(nd)=real(k-1)

       x(nd+1)=real(i-1)+0.5d0
       y(nd+1)=real(j-1)+0.5d0
       z(nd+1)=real(k-1)

       x(nd+2)=real(i-1)
       y(nd+2)=real(j-1)+0.5d0
       z(nd+2)=real(k-1)+0.5d0

       x(nd+3)=real(i-1)+0.5d0
       y(nd+3)=real(j-1)
       z(nd+3)=real(k-1)+0.5d0

       x(nd+4)=real(i-1)+0.25d0
       y(nd+4)=real(j-1)+0.25d0
       z(nd+4)=real(k-1)+0.25d0

       x(nd+5)=real(i-1)+0.25d0
       y(nd+5)=real(j-1)+0.75d0
       z(nd+5)=real(k-1)+0.75d0

       x(nd+6)=real(i-1)+0.75d0
       y(nd+6)=real(j-1)+0.25d0
       z(nd+6)=real(k-1)+0.75d0

       x(nd+7)=real(i-1)+0.75d0
       y(nd+7)=real(j-1)+0.75d0
       z(nd+7)=real(k-1)+0.25d0

       nd=nd+8
      enddo
      enddo
      enddo
      n=nd-1

c --- Make IS
      do i=1,n
       is(i)=1
      enddo

c --- Angstrom, ac: lattice constant
      do i=1,n
       x(i)=x(i)*ac
       y(i)=y(i)*ac
       z(i)=z(i)*ac
      enddo

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

      end
c
      subroutine zincblende
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'zincblende is started.'

      nd=1
      do i=1,nx
      do j=1,ny
      do k=1,nz
       x(nd)=real(i-1)
       y(nd)=real(j-1)
       z(nd)=real(k-1)
       is(nd)=1

       x(nd+1)=real(i-1)+0.5d0
       y(nd+1)=real(j-1)+0.5d0
       z(nd+1)=real(k-1)
       is(nd+1)=1

       x(nd+2)=real(i-1)
       y(nd+2)=real(j-1)+0.5d0
       z(nd+2)=real(k-1)+0.5d0
       is(nd+2)=1

       x(nd+3)=real(i-1)+0.5d0
       y(nd+3)=real(j-1)
       z(nd+3)=real(k-1)+0.5d0
       is(nd+3)=1

       x(nd+4)=real(i-1)+0.25d0
       y(nd+4)=real(j-1)+0.25d0
       z(nd+4)=real(k-1)+0.25d0
       is(nd+4)=2

       x(nd+5)=real(i-1)+0.25d0
       y(nd+5)=real(j-1)+0.75d0
       z(nd+5)=real(k-1)+0.75d0
       is(nd+5)=2

       x(nd+6)=real(i-1)+0.75d0
       y(nd+6)=real(j-1)+0.25d0
       z(nd+6)=real(k-1)+0.75d0
       is(nd+6)=2

       x(nd+7)=real(i-1)+0.75d0
       y(nd+7)=real(j-1)+0.75d0
       z(nd+7)=real(k-1)+0.25d0
       is(nd+7)=2

       nd=nd+8
      enddo
      enddo
      enddo
      n=nd-1

c --- Reverse IS
c      do i=1,n
c       if(is(i).eq.1) is(i)=3
c       if(is(i).eq.2) is(i)=4
c      enddo
c      do i=1,n
c       if(is(i).eq.3) is(i)=2
c       if(is(i).eq.4) is(i)=1
c      enddo

c --- Angstrom, ac: lattice constant
      do i=1,n
       x(i)=x(i)*ac
       y(i)=y(i)*ac
       z(i)=z(i)*ac
      enddo

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

      end
c
      subroutine sic2h
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'sic2h is started.'

c Under construction

      end
c
      subroutine sic4h
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'sic4h is started.'

      nb=8
      allocate(rx(nb),ry(nb),rz(nb))

      r13=1.0d0/3.0d0
      r23=2.0d0/3.0d0
      r2=sqrt(2.0d0)
      r3=sqrt(3.0d0)

      a=ac
      c=cc
      u=0.18742d0

      if(idis.eq.1) write(6,*) 'ac =',ac
      if(idis.eq.1) write(6,*) 'a  =',a
      if(idis.eq.1) write(6,*) 'c  =',c
      if(idis.eq.1) write(6,*) 'u  =',u

c Primitive vector
      px(1)=a 
      py(1)=0.0d0 
      pz(1)=0.0d0 

      px(2)=-a*0.5d0 
      py(2)=a*r3*0.5d0 
      pz(2)=0.0d0 

      px(3)=0.0d0 
      py(3)=0.0d0 
      pz(3)=c 

c Relative coordinate & atom type
      rx(1)=0.0d0
      ry(1)=0.0d0
      rz(1)=0.0d0
      is(1)=1

      rx(2)=0.0d0
      ry(2)=0.0d0
      rz(2)=0.5d0
      is(2)=1

      rx(3)=r13
      ry(3)=r23
      rz(3)=0.25d0
      is(3)=1

      rx(4)=r23
      ry(4)=r13
      rz(4)=0.75d0
      is(4)=1

      rx(5)=rx(1)
      ry(5)=ry(1)
      rz(5)=rz(1)+u
      is(5)=2

      rx(6)=rx(2)
      ry(6)=ry(2)
      rz(6)=rz(2)+u
      is(6)=2

      rx(7)=rx(3)
      ry(7)=ry(3)
      rz(7)=rz(3)+u
      is(7)=2

      rx(8)=rx(4)
      ry(8)=ry(4)
      rz(8)=rz(4)+u
      is(8)=2

c --- Make x,y,z coordinates
      call mkxyz
      if(idis.eq.1) write(6,*) 'mkxyz has been finished.'

c --- Periodic boundary condition
      dx=-1.0d-6
      dy=0.0d0
      dz=0.0d0
      xmin=0.0d0+dx
      xmax=nx*a+dx
      ymin=0.0d0+dy
      ymax=ny*a*r3*0.5d0+dy
      zmin=0.0d0+dz
      zmax=nz*c+dz
      xlen=xmax-xmin
      ylen=ymax-ymin
      zlen=zmax-zmin

      do i=1,n
       if(x(i).gt.xmax) x(i)=x(i)-xlen
       if(x(i).lt.xmin) x(i)=x(i)+xlen
       if(y(i).gt.ymax) y(i)=y(i)-ylen
       if(y(i).lt.ymin) y(i)=y(i)+ylen
       if(z(i).gt.zmax) z(i)=z(i)-zlen
       if(z(i).lt.zmin) z(i)=z(i)+zlen
      enddo

c --- Cut out MD box 
c      call cutbox
c     if(idis.eq.1) write(6,*) 'cutbox has been finished.'

c --- Check
c      write(6,*) 'xmin,xmax:',xmin,xmax
c      write(6,*) 'ymin,ymax:',ymin,ymax
c      write(6,*) 'zmin,zmax:',zmin,zmax
c      do i=1,n
c       write(6,100) i,x(i),y(i),z(i)
c      enddo
c100   format(i6,3e15.7)

      end
c
      subroutine sic6h
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'sic6h is started.'

c Under construction

      end
c
      subroutine sicwire
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: isw
      doubleprecision,dimension(:),allocatable ::
     & wx,wy,wz

      if(idis.eq.1) write(6,*) 'sicwire is started.'

      nd=1
      do i=1,nx
      do j=1,ny
      do k=1,nz
       x(nd)=real(i-1)
       y(nd)=real(j-1)
       z(nd)=real(k-1)
       is(nd)=1

       x(nd+1)=real(i-1)+0.5d0
       y(nd+1)=real(j-1)+0.5d0
       z(nd+1)=real(k-1)
       is(nd+1)=1

       x(nd+2)=real(i-1)
       y(nd+2)=real(j-1)+0.5d0
       z(nd+2)=real(k-1)+0.5d0
       is(nd+2)=1

       x(nd+3)=real(i-1)+0.5d0
       y(nd+3)=real(j-1)
       z(nd+3)=real(k-1)+0.5d0
       is(nd+3)=1

       x(nd+4)=real(i-1)+0.25d0
       y(nd+4)=real(j-1)+0.25d0
       z(nd+4)=real(k-1)+0.25d0
       is(nd+4)=2

       x(nd+5)=real(i-1)+0.25d0
       y(nd+5)=real(j-1)+0.75d0
       z(nd+5)=real(k-1)+0.75d0
       is(nd+5)=2

       x(nd+6)=real(i-1)+0.75d0
       y(nd+6)=real(j-1)+0.25d0
       z(nd+6)=real(k-1)+0.75d0
       is(nd+6)=2

       x(nd+7)=real(i-1)+0.75d0
       y(nd+7)=real(j-1)+0.75d0
       z(nd+7)=real(k-1)+0.25d0
       is(nd+7)=2

       nd=nd+8
      enddo
      enddo
      enddo
      n=nd-1

c --- Cutting
c --- square
c      a=0.0*nx
c --- octagon
      a=0.25*nx
      b=nx-a
      c=2.0*nx-a
      allocate(wx(n),wy(n),wz(n),isw(n))

      nd=0
      do i=1,n
       if(y(i).ge.-x(i)+a) then
       if(y(i).lt. x(i)+b) then
       if(y(i).lt.-x(i)+c) then
       if(y(i).gt. x(i)-b) then
        nd=nd+1
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        isw(nd)=is(i)
       endif
       endif
       endif
       endif
      enddo
      n=nd
      write(6,*) 'n =',n
      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=isw(i)
      enddo

c --- Angstrom, ac: lattice constant
      do i=1,n
       x(i)=x(i)*ac
       y(i)=y(i)*ac
       z(i)=z(i)*ac
      enddo

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

c --- Translation
      do i=1,n
       x(i)=x(i)+xv
       y(i)=y(i)+yv
       z(i)=z(i)+zv
      enddo

c --- Periodic boundary condition
      xmax=nx*ac+xv*2.0d0
      xmin=0.0d0
      ymax=ny*ac+yv*2.0d0
      ymin=0.0d0
      zmax=nz*ac+zv*2.0d0
      zmin=0.0d0

      end
c
      subroutine cuwire
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: isw
      doubleprecision,dimension(:),allocatable ::
     & wx,wy,wz

      if(idis.eq.1) write(6,*) 'cuwire is started.'

      nd=1
      do i=1,nx
      do j=1,ny
      do k=1,nz
       x(nd)=real(i-1)
       y(nd)=real(j-1)
       z(nd)=real(k-1)

       x(nd+1)=real(i-1)+0.5d0
       y(nd+1)=real(j-1)+0.5d0
       z(nd+1)=real(k-1)

       x(nd+2)=real(i-1)
       y(nd+2)=real(j-1)+0.5d0
       z(nd+2)=real(k-1)+0.5d0

       x(nd+3)=real(i-1)+0.5d0
       y(nd+3)=real(j-1)
       z(nd+3)=real(k-1)+0.5d0

       nd=nd+4
      enddo
      enddo
      enddo
      n=nd-1

c --- Make IS
      do i=1,n
       is(i)=1
      enddo

c --- Cutting
c --- square
c      a=0.0*nx
c --- octagon
      a=0.25*nx
      b=nx-a
      c=2.0*nx-a
      allocate(wx(n),wy(n),wz(n),isw(n))

      nd=0
      do i=1,n
       if(y(i).ge.-x(i)+a) then
       if(y(i).lt. x(i)+b) then
       if(y(i).lt.-x(i)+c) then
       if(y(i).gt. x(i)-b) then
        nd=nd+1
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        isw(nd)=is(i)
       endif
       endif
       endif
       endif
      enddo
      n=nd
      write(6,*) 'n =',n
      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=isw(i)
      enddo

c --- Angstrom, ac: lattice constant
      do i=1,n
       x(i)=x(i)*ac
       y(i)=y(i)*ac
       z(i)=z(i)*ac
      enddo

c --- Periodic boundary condition
      xmax=nx*ac
      xmin=0.0d0
      ymax=ny*ac
      ymin=0.0d0
      zmax=nz*ac
      zmin=0.0d0

c --- Translation
      do i=1,n
       x(i)=x(i)+xv
       y(i)=y(i)+yv
       z(i)=z(i)+zv
      enddo

c --- Periodic boundary condition
      xmax=nx*ac+xv*2.0d0
      xmin=0.0d0
      ymax=ny*ac+yv*2.0d0
      ymin=0.0d0
      zmax=nz*ac+zv*2.0d0
      zmin=0.0d0

      end
c
      subroutine fcc100wire
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc100wire is started.'

c --- Make fcc100
      call fcc100

c --- Translation
      do i=1,n
       x(i)=x(i)+xv
       y(i)=y(i)+yv
       z(i)=z(i)+zv
      enddo

c --- Periodic boundary condition
      xmax=nx*ac+xv*2.0d0
      xmin=0.0d0
      ymax=ny*ac+yv*2.0d0
      ymin=0.0d0
      zmax=nz*ac+zv*2.0d0
      zmin=0.0d0

      end
c
      subroutine fcc100h
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc100h is started.'

c --- Make fcc100
      call fcc100
      if(idis.eq.1) write(6,*) 'fcc100 has been finished.'

c --- Insert H
c      call addhran 
c      if(idis.eq.1) write(6,*) 'addhran has been finished.'

c --- Insert H randomly
      call addhran_r
      if(idis.eq.1) write(6,*) 'addhran has been finished.'

      end
c
      subroutine rs100vac
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: iw
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'rs100vac is started.'

c --- Set array
      nmx=nx*ny*nz*8
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn),iw(nn))

      do i=1,nn
       iw(i)=0
       wx(i)=0.0d0
       wy(i)=0.0d0
       wz(i)=0.0d0
      enddo

c --- Make rocksalt
      call rs0
      if(idis.eq.1) write(6,*) 'rs0 has been finished.'

      nd=0
      do i=1,n
       if(is(i).eq.2) then
        is(i)=0
        nd=nd+1
        if(nd.ge.nh) goto 1000
       endif
      enddo
1000  continue

c --- Remove H atoms
      nd=1
      do i=1,n
       if(is(i).gt.0) then
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        iw(nd)=is(i)
        nd=nd+1
       endif 
      enddo
      n=nd-1

      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=iw(i)
      enddo

      end
c
      subroutine rs100vacr
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: iw,ir
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'rs100vacr is started.'

c --- Set array
      nmx=nx*ny*nz*8
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn),iw(nn),ir(nn))

      do i=1,nn
       iw(i)=0
       wx(i)=0.0d0
       wy(i)=0.0d0
       wz(i)=0.0d0
      enddo

c --- Make rocksalt
      call rs0
      if(idis.eq.1) write(6,*) 'rs0 has been finished.'

      iseed=12345
      call randomint(n,ir,iseed)

      nhalf=n/2
      nrm=nhalf-nh
      if(nrm.lt.0) then
       write(6,*) 'Error in rs100vacr.'
       write(6,*) 'n,nhalf,nrm =',n,nhalf,nrm
       stop
      endif

      nd=0
      do i=1,n
       if(is(ir(i)).eq.2) then
        is(ir(i))=0
        nd=nd+1
        if(nd.ge.nrm) goto 1000
       endif
      enddo
1000  continue

c --- Remove H atoms
      nd=1
      do i=1,n
       if(is(i).gt.0) then
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        iw(nd)=is(i)
        nd=nd+1
       endif 
      enddo
      n=nd-1

      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=iw(i)
      enddo

c --- Check
      n1=0
      n2=0
      do i=1,n
       if(is(i).eq.1) n1=n1+1
       if(is(i).eq.2) n2=n2+1
      enddo
      if(idis.eq.1) write(6,*) '# of type 1 =',n1
      if(idis.eq.1) write(6,*) '# of type 2 =',n2
       
      end
c
      subroutine rs100intsub
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: iw,ir
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'rs100intsub is started.'

c --- Set array
      nmx=nx*ny*nz*8
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn),iw(nn),ir(nn))

      do i=1,nn
       iw(i)=0
       wx(i)=0.0d0
       wy(i)=0.0d0
       wz(i)=0.0d0
      enddo

c --- Make rocksalt
      call rs0
      if(idis.eq.1) write(6,*) 'rs0 has been finished.'


c --- Interstitials
      if(idis.eq.1) write(6,*) 'Make interstitials.'
      iseed=12345
      call randomint(n,ir,iseed)

      nhalf=n/2
      nrm=nhalf-nh
      if(nrm.lt.0) then
       write(6,*) 'Error in rs100intsub.'
       write(6,*) 'n,nhalf,nrm =',n,nhalf,nrm
       stop
      endif

      nd=0
      do i=1,n
       if(is(ir(i)).eq.2) then
        is(ir(i))=0
        nd=nd+1
        if(nd.ge.nrm) goto 1000
       endif
      enddo
1000  continue

c --- Remove H atoms
      nd=1
      do i=1,n
       if(is(i).gt.0) then
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        iw(nd)=is(i)
        nd=nd+1
       endif 
      enddo
      n=nd-1
      if(idis.eq.0) write(6,*) 'n =',n

      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=iw(i)
      enddo

c --- Recount
      n1=0
      n2=0
      do i=1,n
       if(is(i).eq.1) n1=n1+1
       if(is(i).eq.2) n2=n2+1
      enddo
      if(idis.eq.1) write(6,*) '# of type 1 =',n1
      if(idis.eq.1) write(6,*) '# of type 2 =',n2
       
c --- Substitutional
      if(idis.eq.1) write(6,*) 'Make substitutionals.'
      iseed=54321
      call randomint(n,ir,iseed)

c --- Check
c      write(6,*) 'nsub =',nsub
      nrm=n1-nsub
      if(nrm.lt.0) then
       write(6,*) 'Error in rs100intsub.'
       write(6,*) 'n1,nrm =',n1,nsub
       stop
      endif

      nd=0
      do i=1,n
      i1=ir(i)
       if(is(i1).eq.1.and.nd.lt.nsub) then
        is(i1)=3
        nd=nd+1
c        write(6,*) i,nd,ir(i1)
       endif
      enddo 

c --- Check
      n1=0
      n2=0
      n3=0
      do i=1,n
       if(is(i).eq.1) n1=n1+1
       if(is(i).eq.2) n2=n2+1
       if(is(i).eq.3) n3=n3+1
      enddo
      if(idis.eq.1) write(6,*) '# of type 1 =',n1
      if(idis.eq.1) write(6,*) '# of type 2 =',n2
      if(idis.eq.1) write(6,*) '# of type 3 =',n3
       
      end
c
      subroutine fcc0alloy
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: iw,ir
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'fcc0ally is started.'

c --- Set array
      nmx=nx*ny*nz*8
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn),iw(nn),ir(nn))

      do i=1,nn
       iw(i)=0
       wx(i)=0.0d0
       wy(i)=0.0d0
       wz(i)=0.0d0
      enddo

c --- Make fcc0
      call fcc0
      if(idis.eq.1) write(6,*) 'fcc0 has been finished.'

      iseed=12345
      call randomint(n,ir,iseed)

      do i=1,nsub
       is(ir(i))=2
      enddo 

c      nd=0
c      do i=1,n
c       if(is(ir(i)).eq.2) then
c        is(ir(i))=0
c        nd=nd+1
c        if(nd.ge.nh) goto 1000
c       endif
c      enddo
c1000  continue

c --- Remove H atoms
c      nd=1
c      do i=1,n
c       if(is(i).gt.0) then
c        wx(nd)=x(i)
c        wy(nd)=y(i)
c        wz(nd)=z(i)
c        iw(nd)=is(i)
c        nd=nd+1
c       endif 
c      enddo
c      n=nd-1
c
c      do i=1,n
c       x(i)=wx(i)
c       y(i)=wy(i)
c       z(i)=wz(i)
c       is(i)=iw(i)
c      enddo

c --- Check
      n1=0
      n2=0
      do i=1,n
       if(is(i).eq.1) n1=n1+1
       if(is(i).eq.2) n2=n2+1
      enddo
      if(idis.eq.1) write(6,*) '# of type 1 =',n1
      if(idis.eq.1) write(6,*) '# of type 2 =',n2
       
      end
c
      subroutine randomint(m,ir,iseed)
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: iw,iu
      doubleprecision,dimension(:),allocatable :: x
      dimension ir(m)

c --- Check
c      write(6,*) 'iseed =',iseed
c      write(6,*) 'm =',m

      nd=m*100
      allocate(iw(nd),iu(nd),x(nd))

      do i=1,nd
       iu(i)=0
       x(i)=0.0d0
c       write(6,*) i,iu(i),x(i)
      enddo

      call urand2(nd,x,iseed)
c      write(6,*) 'urand2 finished.'

      do i=1,nd
       iw(i)=m*x(i)
c       write(6,*) i,x(i),iw(i)
       if(iw(i).eq.0) iw(i)=m
      enddo

c --- Check
c      iwmin=iw(1)
c      iwmax=iw(1)
c      do i=1,nd
c       if(iw(i).lt.iwmin) iwmin=iw(i)
c       if(iw(i).gt.iwmax) iwmax=iw(i)
c      enddo
c      write(6,*) 'nd =',nd
c      write(6,*) 'iwmin =',iwmin
c      write(6,*) 'iwmax =',iwmax

      ir(1)=iw(1)
      iu(ir(1))=1

      jp=2
      do ip=2,nd
       ipr=iw(ip)
       jpr=ir(jp)
       if(ipr.ne.jpr.and.iu(ipr).eq.0) then
        ir(jp)=ipr
        iu(ipr)=1
        jp=jp+1
        if(jp.gt.m) goto 1000
       endif
      enddo
1000  continue
c      ir(m)=m

c --- Check
c      do i=1501,1600
c       write(6,*) i,ir(i)
c      enddo

c --- Check
      do i=1,m
       if(ir(i).eq.0) then
        write(6,*) 'Error in randomint.',i,ir(i)
        stop
       endif
c       write(6,*) i,ir(i)
      enddo

      end
c
      subroutine fcc111
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc111 is started.'

      r2=sqrt(2.0d0)
      r3=sqrt(3.0d0)

      nd=1
      do i=1,nx
      do j=1,ny
      do k=1,nz
       x(nd)=(i-1)*r3/r2
       y(nd)=(j-1)*r3
       z(nd)=(k-1)*r2*0.5d0

       x(nd+1)=x(nd)+r3/r2*0.5d0
       y(nd+1)=y(nd)
       z(nd+1)=z(nd)+r2*0.25d0

       x(nd+2)=x(nd)+1.0d0/r2/r3*0.5d0
       y(nd+2)=y(nd)+r3/3.0d0
       z(nd+2)=z(nd)+r2*0.25d0

       x(nd+3)=x(nd+2)+r3/r2*0.5d0
       y(nd+3)=y(nd)+r3/3.0d0
       z(nd+3)=z(nd+2)+r2*0.25d0

       x(nd+4)=x(nd)+1.0d0/r2/r3
       y(nd+4)=y(nd)+2.0d0*r3/3.0d0
       z(nd+4)=z(nd)+r2*0.5d0

       x(nd+5)=x(nd+4)+r3/r2*0.5d0
       y(nd+5)=y(nd)+2.0d0*r3/3.0d0
       z(nd+5)=z(nd+4)-r2*0.25d0

       nd=nd+6
      enddo
      enddo
      enddo
      n=nd-1

c --- Make IS
      do i=1,n
       is(i)=1
      enddo

c --- Angstrom, ac: lattice constant
      do i=1,n
       x(i)=x(i)*ac
       y(i)=y(i)*ac
       z(i)=z(i)*ac
      enddo

c --- Periodic boundary condition
      xmax=nx*ac*r3/r2
      xmin=0.0d0
      ymax=ny*ac*r3
      ymin=0.0d0
      zmax=nz*ac*r2*0.5d0
      zmin=0.0d0

      end
c
      subroutine fcc111h
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc111h is started.'

c --- Make fcc111
      call fcc111
      if(idis.eq.1) write(6,*) 'fcc111 has been finished.'

c --- Insert H
c      call addhran 
c      if(idis.eq.1) write(6,*) 'addhran has been finished.'

c --- Insert H randomly
      call addhran_r
      if(idis.eq.1) write(6,*) 'addhran has been finished.'

      end
c
      subroutine fcc111r
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc111h is started.'

c --- Make fcc100
      call fcc100
      if(idis.eq.1) write(6,*) 'fcc100 has been finished.'

c --- Dislocate
      do i=1,n
       x1=(x(i)-z(i))/sqrt(2.0d0)
       z(i)=(x(i)+z(i))/sqrt(2.0d0)
       x(i)=x1
      enddo

c --- Dislocate
      do i=1,n
       y1=(sqrt(2.0d0)*y(i)-z(i))/sqrt(3.0d0)
       z(i)=(y(i)+sqrt(2.0d0)*z(i))/sqrt(3.0d0)
       y(i)=y1
      enddo

      end
c
      subroutine fcc111e
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'fcc111e is started.'

c --- Make fcc111
      call fcc111
      if(idis.eq.1) write(6,*) 'fcc111 has been finished.'

c --- Set array
      nmx=nx*ny*nz*6+nh
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn))

      xc=(xmax-xmin)*0.5d0
      yc=(ymax-ymin)*0.5d0
      clen=(ymax-ymin)*0.5d0
      dx=ac*0.4d0
c      dx=ac*0.2d0
      dd=-ac*0.0d0

      wx0=xc-dx
      wy0=yc-clen*0.5d0-dd
c      wy0=ymin+ac*10.0d0
      wx1=xc+dx
      wy1=yc+clen*0.5d0+dd
c      wy1=ymax-ac*10.0d0
      if(idis.eq.1) write(6,*) 'wx0,wy0,wx1,wy1:'
      if(idis.eq.1) write(6,100) wx0,wy0,wx1,wy1
100   format(4e15.7)

c      wx0=0.0d0
c      wy0=0.0d0
c      wx1=0.0d0
c      wy1=0.0d0

      do i=1,n
       if(x(i).gt.wx0.and.y(i).gt.wy0.and.
     &  x(i).lt.wx1.and.y(i).lt.wy1) is(i)=0 
      enddo

      nd=0
      do i=1,n
       if(is(i).gt.0) then
        nd=nd+1
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
       endif
      enddo
      if(idis.eq.1) write(6,*) '# of atoms:',nd

      n=nd
      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=1
      enddo

      end

      subroutine fcc111ef
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'fcc111ef is started.'

c --- Make fcc111
      call fcc111
      if(idis.eq.1) write(6,*) 'fcc111 has been finished.'

c --- Set array
      nmx=nx*ny*nz*6*mx*my+nh
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn))

      xc=(xmax-xmin)*0.5d0
      yc=(ymax-ymin)*0.5d0
      clen=(ymax-ymin)*0.5d0
      dx=ac*0.4d0
c      dx=ac*0.2d0
      dd=-ac*0.0d0

      wx0=xc-dx
      wy0=yc-clen*0.5d0-dd
c      wy0=ymin+ac*10.0d0
      wx1=xc+dx
      wy1=yc+clen*0.5d0+dd
c      wy1=ymax-ac*10.0d0
      if(idis.eq.1) write(6,*) 'wx0,wy0,wx1,wy1:'
      if(idis.eq.1) write(6,100) wx0,wy0,wx1,wy1
100   format(4e15.7)

c      wx0=0.0d0
c      wy0=0.0d0
c      wx1=0.0d0
c      wy1=0.0d0

      do i=1,n
       if(x(i).gt.wx0.and.y(i).gt.wy0.and.
     &  x(i).lt.wx1.and.y(i).lt.wy1) is(i)=0 
      enddo

      nd=0
      do i=1,n
       if(is(i).gt.0) then
        nd=nd+1
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
       endif
      enddo
      if(idis.eq.1) write(6,*) '# of atoms:',nd

      n=nd
      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=1
      enddo

c --- Unit cell is copied in x- and y-direction. 
      xlen=xmax-xmin
      ylen=ymax-ymin

      id=1
      do i=1,mx
      do j=1,my
      do k=1,n
       x(id)=x(k)+(i-1)*xlen
       y(id)=y(k)+(j-1)*ylen
       z(id)=z(k)
       is(id)=1
       id=id+1
      enddo
      enddo
      enddo
      n=n*mx*my

c --- MD box is modified.
      xmax=xmin+xlen*mx
      ymax=ymin+ylen*my

      end
c
      subroutine fcc111sc
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc111sc is started.'

c --- Make fcc111
      call fcc111
      if(idis.eq.1) write(6,*) 'fcc111 has been finished.'

c --- Burger's vector
      r2=sqrt(2.0d0)
c      b2=ac/r2*0.5d0
      b2=ac/r2
c      b2=10.0d0

      xlen=xmax-xmin
      ylen=ymax-ymin
      xc=xlen*0.5d0
      yc=ylen*0.5d0
      y0=yc-ylen*0.25d0
      y1=yc+ylen*0.25d0
      xw=xlen*0.25d0
      x0=xc-xw
      x1=xc+xw
      if(idis.eq.1) write(6,*) 'y0,y1:',y0,y1
      if(idis.eq.1) write(6,*) 'x0,x1:',x0,x1

c --- Shift to generate screw dislocation
      zs=b2/(yc-y0)
      do i=1,n
      if(x(i).gt.x0.and.x(i).lt.x1) then
      if(y(i).gt.y0.and.y(i).lt.yc) then
       yd=y(i)-y0
       z(i)=z(i)+zs*yd
      endif
      endif
      if(x(i).gt.x0.and.x(i).lt.x1) then
      if(y(i).gt.yc.and.y(i).lt.y1) then
       yd=y(i)-y1
       z(i)=z(i)+zs*yd
      endif
      endif
      enddo

      end

      subroutine fcc111scqd
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'fcc111scqd is started.'

c --- Make fcc111
      call fcc111
      if(idis.eq.1) write(6,*) 'fcc111 has been finished.'

c --- Burger's vector
      r2=sqrt(2.0d0)
      b25=ac/r2*0.5d0
      b2=ac/r2
c      b2=10.0d0

c --- Set array
      nmx=nx*ny*nz*6+nh
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn))

c --- Set quadrupole position
      xlen=xmax-xmin
      ylen=ymax-ymin
      xc=xlen*0.5d0
      yc=ylen*0.5d0
      xq=xlen*0.25d0
      yq=ylen*0.25d0
c --- version 1
c      x0=xmin+xq
c      y0=ymin+yq
c      x1=xmin+3.0d0*xq
c      y1=ymin+3.0d0*yq
c --- version 2
      qlen5=b2*5.0d0
      x0=xc-qlen5
      y0=yc-qlen5
      x1=xc+qlen5
      y1=yc+qlen5
      if(idis.eq.1) write(6,*) 'b =',b2
      if(idis.eq.1) write(6,*) 'x0,y0:',x0,y0
      if(idis.eq.1) write(6,*) 'x1,y1:',x1,y1
      if(idis.eq.1) write(6,*) 'xc,yc:',xc,yc

c --- Generate screw dislocation
      nc=0
      do i=1,n
      if(x(i).gt.x0.and.x(i).lt.xc) then
      if(y(i).gt.y0.and.y(i).lt.yc) then
c       z(i)=z(i)-b2
c       z(i)=z(i)-b2*0.5d0
       nc=nc+1
      endif
      endif
      if(x(i).gt.x0.and.x(i).lt.xc) then
      if(y(i).gt.yc.and.y(i).lt.y1) then
       z(i)=z(i)+b2*0.5d0
       nc=nc+1
      endif
      endif
      if(x(i).gt.xc.and.x(i).lt.x1) then
      if(y(i).gt.y0.and.y(i).lt.yc) then
       z(i)=z(i)-b2*0.25d0
       nc=nc+1
      endif
      endif
      if(x(i).gt.xc.and.x(i).lt.x1) then
      if(y(i).gt.yc.and.y(i).lt.y1) then
       z(i)=z(i)+b2*0.25d0
       nc=nc+1
      endif
      endif
      enddo
      if(idis.eq.1) write(6,*) 'nc =',nc

      nc=0
      do i=1,n
       if(z(i).lt.zmin) then
       if(z(i).lt.zmin.or.z(i).gt.zmax) then
        is(i)=0
        nc=nc+1
       endif
       endif
      enddo
      if(idis.eq.1) write(6,*) 'nc =',nc

      nd=0
      do i=1,n
       if(is(i).gt.0) then
        nd=nd+1
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
       endif
      enddo
      if(idis.eq.1) write(6,*) '# of atoms:',nd

      n=nd
      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=1
      enddo

      end

      subroutine fcch
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Read LAMMPS dump file
      call readdump
      if(idis.eq.1) write(6,*) 'readdump has been finished.'

c --- Insert H
      call addhran_r 
      if(idis.eq.1) write(6,*) 'addhran_r has been finished.'

      end

      subroutine indent
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Make fcc100
      call fcc100
      if(idis.eq.1) write(6,*) 'fcc100 has been finished.'

c --- Bottom
      dy=rc*1.1d0
      y0=ymin
      y1=y0+dy
      do i=1,n
       if(y(i).ge.y0.and.y(i).lt.y1) is(i)=2
      enddo
      nty=nty+1

      end

      subroutine indentz
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Make fcc100
      call fcc100
      if(idis.eq.1) write(6,*) 'fcc100 has been finished.'

c --- Bottom
      db=rc*1.1d0
c      b0=ymin
c      b1=y0+dy
      b0=zmin
      b1=z0+db
      do i=1,n
c       if(y(i).ge.b0.and.y(i).lt.b1) is(i)=2
       if(z(i).ge.b0.and.z(i).lt.b1) is(i)=2
      enddo
c      nty=nty+1

      end

      subroutine indentzh
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Make fcc100
      call fcc100
      if(idis.eq.1) write(6,*) 'fcc100 has been finished.'

c --- Insert H
      buf=zmin
      zmin=zmin+rc*1.1d0
      call addhran_r 
      if(idis.eq.1) write(6,*) 'addhran_r has been finished.'
      zmin=buf

c --- Bottom
      db=rc*1.1d0
c      b0=ymin
c      b1=y0+dy
      b0=zmin
      b1=b0+db
      do i=1,n
c       if(y(i).ge.b0.and.y(i).lt.b1) is(i)=2
       if(z(i).ge.b0.and.z(i).lt.b1) is(i)=3
      enddo
c      nty=nty+1

      end

      subroutine indentzb
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Make zincblende
      call zincblende
      if(idis.eq.1) write(6,*) 'zincblende has been finished.'

c --- Bottom
      dy=rc*1.1d0
      y0=ymin
      y1=y0+dy
      do i=1,n
       if(y(i).ge.y0.and.y(i).lt.y1) then
        if(is(i).eq.1) is(i)=3
        if(is(i).eq.2) is(i)=4
       endif
      enddo
      nty=nty+2

      end

      subroutine indentrsz
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Make rocksalt
      call rs0
      if(idis.eq.1) write(6,*) 'rockralt has been finished.'

c --- Bottom
      dz=rc*1.1d0
      z0=zmin
      z1=z0+dz
      do i=1,n
       if(z(i).ge.z0.and.z(i).lt.z1) then
        if(is(i).eq.1) is(i)=3
        if(is(i).eq.2) is(i)=4
       endif
      enddo

      end

      subroutine indentrsvacz
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:),allocatable :: iw,ir
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

c --- Make rocksalt
      call rs0
      if(idis.eq.1) write(6,*) 'rocksalt has been finished.'

c --- Bottom
      dz=rc*1.1d0
      z0=zmin
      z1=z0+dz
      do i=1,n
       if(z(i).ge.z0.and.z(i).lt.z1) then
        if(is(i).eq.1) is(i)=3
        if(is(i).eq.2) is(i)=4
       endif
      enddo

c --- Make vacancies

c --- Set array
      nmx=nx*ny*nz*8
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn),iw(nn),ir(nn))

      do i=1,nn
       iw(i)=0
       wx(i)=0.0d0
       wy(i)=0.0d0
       wz(i)=0.0d0
      enddo

      iseed=12345
      call randomint(n,ir,iseed)

      nd=0
      do i=1,n
       if(is(ir(i)).eq.2) then
        is(ir(i))=0
        nd=nd+1
        if(nd.ge.nh) goto 1000
       endif
      enddo
1000  continue

c --- Remove H atoms
      nd=1
      do i=1,n
       if(is(i).gt.0) then
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        iw(nd)=is(i)
        nd=nd+1
       endif 
      enddo
      n=nd-1

      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=iw(i)
      enddo

c --- Check
      n1=0
      n2=0
      n3=0
      n4=0
      do i=1,n
       if(is(i).eq.1) n1=n1+1
       if(is(i).eq.2) n2=n2+1
       if(is(i).eq.3) n3=n3+1
       if(is(i).eq.4) n4=n4+1
      enddo
      if(idis.eq.1) write(6,*) '# of type 1 =',n1
      if(idis.eq.1) write(6,*) '# of type 2 =',n2
      if(idis.eq.1) write(6,*) '# of type 3 =',n3
      if(idis.eq.1) write(6,*) '# of type 4 =',n4
       
      end

      subroutine indentzbz
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Make zincblende
      call zincblende
      if(idis.eq.1) write(6,*) 'zincblende has been finished.'

c --- Bottom
      dz=rc*1.1d0
      z0=zmin
      z1=z0+dz
      do i=1,n
       if(z(i).ge.z0.and.z(i).lt.z1) then
        if(is(i).eq.1) is(i)=3
        if(is(i).eq.2) is(i)=4
       endif
      enddo

      end

      subroutine dump2data
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- Selecter
c --- 0: dump style = atom
c --- 1: dump style = xyz
c --- 2: dump style = custom type x y z
      nds=0

c --- Read LAMMPS dump file
      if(nds.eq.0) then

c --- dump style = atom
      call readdump

      elseif(nds.eq.1) then

c --- dump style = xyz
      call readdump1

      elseif(nds.eq.2) then

c --- dump style = custom type x y z
      call readdump2

      endif

      if(idis.eq.1) write(6,*) 'readdump has been finished.'

      end

c --- dump style = atom
      subroutine readdump
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      character (len=80) :: com

      open(10,file='res0',status='old')

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

c --- dump style = xyz
      subroutine readdump1
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      character (len=80) :: com

      open(10,file='res0',status='old')

      read(10,*) n
      if(idis.eq.1) write(6,*) 'n =',n
      read(10,100) com
      if(idis.eq.1) write(6,100) com
      do i=1,n
       read(10,*) is(i),x(i),y(i),z(i)
      enddo

      close(10)
100   format(a80)

c --- Periodic boundary condition
      r2=sqrt(2.0d0)
      r3=sqrt(3.0d0)
      xmax=nx*ac*r3/r2
      xmin=0.0d0
      ymax=ny*ac*r3
      ymin=0.0d0
      zmax=nz*ac*r2*0.5d0
      zmin=0.0d0

      end

c --- dump style = custom type x y z
      subroutine readdump2
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      character (len=80) :: com

      open(10,file='res0',status='old')

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
       read(10,*) is(i),x(i),y(i),z(i)
      enddo

      close(10)
100   format(a80)

      end

      subroutine fcc111e2
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: wx,wy,wz

      if(idis.eq.1) write(6,*) 'fcc111e is started.'

c --- Make fcc111
      call fcc111
      if(idis.eq.1) write(6,*) 'fcc111 has been finished.'

c --- Set array
      nmx=nx*ny*nz*6+nh
      nn=max(nmx,1000)
      allocate(wx(nn),wy(nn),wz(nn))

      xc=(xmax-xmin)*0.5d0
      yc=(ymax-ymin)*0.5d0
      clen=(ymax-ymin)*0.5d0
      dx=ac*0.4d0
c      dx=ac*0.2d0

      wx0=xc-dx
      wy0=yc-clen*0.5d0
c      wy0=ymin+ac*10.0d0
      wx1=xc+dx
      wy1=yc+clen*0.5d0
c      wy1=ymax-ac*10.0d0
      if(idis.eq.1) write(6,*) 'wx0,wy0,wx1,wy1:'
      if(idis.eq.1) write(6,100) wx0,wy0,wx1,wy1
100   format(4e15.7)

c      wx0=0.0d0
c      wy0=0.0d0
c      wx1=0.0d0
c      wy1=0.0d0

      do i=1,n
       if(x(i).gt.wx0.and.y(i).lt.wy0.and.
     &  x(i).lt.wx1) is(i)=0 
       if(x(i).gt.wx0.and.
     &  x(i).lt.wx1.and.y(i).gt.wy1) is(i)=0 
      enddo

      nd=0
      do i=1,n
       if(is(i).gt.0) then
        nd=nd+1
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
       endif
      enddo
      if(idis.eq.1) write(6,*) '# of atoms:',nd

      n=nd
      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=1
      enddo

      end
c
      subroutine screw
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'screw is started.'

      pai=acos(-1.0d0)
      r2=sqrt(2.0d0)
      b=ac/r2
      cc=b*0.5d0/pai
      if(idis.eq.1) write(6,*) 'cc =',cc

      call syssize
      write(6,*) 'syssize has been finished.'

      xc=(x1-x0)*0.5d0
      yc=(y1-y0)*0.5d0

      do i=1,n
       xp=x(i)-xc
       yp=y(i)-yc
       if(xp.gt.0.0d0.and.yp.gt.0.0d0) then
        theta=atan(yp/xp)
       elseif(xp.lt.0.0d0) then
        theta=atan(yp/xp)+pai
       elseif(xp.gt.0.0d0.and.yp.lt.0.0d0) then
        theta=atan(yp/xp)+pai*2.0d0
       endif
       u3=cc*theta
       z(i)=z(i)+u3
      enddo

      zlen=zmax-zmin

      do i=1,n
       if(z(i).gt.zmax) z(i)=zmax-zlen
       if(z(i).lt.zmin) z(i)=zmax+zlen
      enddo

      end
c
      subroutine addh
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'addh is started.'

      ncx=nx/2
      ncy=ny/2
      ncz=nz/2
      mx=nh**(1.0/3.0)
      my=mx
      mz=mx
      n2=mx/2
      nd=0
      do i=1,mx
      do j=1,my
      do k=1,mz
       nd=nd+1
       ic=n+nd
       x(ic)=ncx+0.5d0+i-1-n2
       y(ic)=ncy+0.5d0+j-1-n2
       z(ic)=ncz+0.5d0+k-1-n2
       is(ic)=2
      enddo
      enddo
      enddo
      n=n+nh

c --- Number of atom type
c      nty=2

      end
c
      subroutine addhran
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: w

      if(idis.eq.1) write(6,*) 'addhran is started.'

      nn=max(nh*3,1000)
      allocate(w(nn))
      w=0.0d0

c --- generate random numbers
      ir=12345
      call urand2(nn,w,ir)

c --- Set hydrogen 
      nd=1
      do i=1,nh
       x(n+i)=w(nd)*xmax 
       y(n+i)=w(nd+1)*ymax 
       z(n+i)=w(nd+2)*zmax 
       is(n+i)=2
       nd=nd+3
c      write(6,*) i,x(n+i),y(n+i),z(n+i)
      enddo
      n=n+nh

c --- Number of atom type
c      nty=2

      end
c
      subroutine addhran_r
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: w

      if(idis.eq.1) write(6,*) 'addhran_r is started.'

      nn=max(nh*3,1000)
      allocate(w(nn))
      w=0.0d0

c --- generate random numbers
      ir=12345
      call urand2(nn,w,ir)

      x0=xmin
      x1=xmax
      y0=ymin
      y1=ymax
      z0=zmin
      z1=zmax
c center along x-axis
c      x0=xmin+xmax*0.25d0
c      x1=xmin+xmax*0.75d0
c right-up side
c      x0=xmin+(xmax-xmin)*0.5d0
c      y0=ymin+(ymax-ymin)*0.5d0

      xlen=x1-x0
      ylen=y1-y0
      zlen=z1-z0
c      write(6,*) xlen,ylen,zlen

c --- Set hydrogen 
      nd=1
      do i=1,nh
c       x(n+i)=w(nd)*xmax 
c       y(n+i)=w(nd+1)*ymax 
c       z(n+i)=w(nd+2)*zmax 
       x(n+i)=x0+w(nd)*xlen 
       y(n+i)=y0+w(nd+1)*ylen 
       z(n+i)=z0+w(nd+2)*zlen 
       is(n+i)=2
       nd=nd+3
c      write(6,*) i,x(n+i),y(n+i),z(n+i)
      enddo
      n=n+nh

      end
c
      subroutine addhran1d
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: w,w0

      if(idis.eq.1) write(6,*) 'addhran1d is started.'

      nn=max(nh*10,1000)
      allocate(w(nn),w0(nn))
      w=0.0d0
      w0=0.0d0

c --- generate random numbers
      ir=12345
      call urand2(nn,w0,ir)
      w=w0

      xlen=xmax-xmin
      ylen=ymax-ymin
      blen=max(xlen,ylen)

      xl=(xmin+dw)/blen
      xu=(xmax-dw)/blen
      yl=(ymin+dw)/blen
      yu=(ymax-dw)/blen

      wl=max(xl,yl)
      wu=min(xu,yu)

      nd=1
      do i=1,nn
       if(w0(i).gt.wl.and.w0(i).lt.wu) then
        w(nd)=w0(i)
        nd=nd+1
       endif
      enddo
      
c --- Set hydrogen
      nd=1
      do i=1,nh
       x(n+i)=w(nd)*xmax 
       y(n+i)=w(nd+1)*ymax 
       z(n+i)=w(nd+2)*zmax 
       is(n+i)=3
       nd=nd+3
c      write(6,*) i,x(n+i),y(n+i),z(n+i)
      enddo
      n=n+nh

      end
c
      subroutine fcc100v
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'fcc100v is started.'

c --- Make fcc100
      call fcc100
      if(idis.eq.1) write(6,*) 'fcc100 has been finished.'

c --- Generate vacancies
      call addvac
      if(idis.eq.1) write(6,*) 'addvac has been finished.'

      end
c
      subroutine addvac
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: wx,wy,wz,w

      if(idis.eq.1) write(6,*) 'addvac is started.'

      nn=max(nv*2,1000)
      allocate(wx(n),wy(n),wz(n))

      allocate(w(nn))
      w=0.0d0

c --- generate random numbers
      ir=12345
      call urand2(nn,w,ir)

      nd=0
      do i=1,n
      if(nd.ge.nv) goto 10
      m=n*w(i)
      if(is(m).gt.0) then
c       write(6,*) nd,m,is(m)
       is(m)=0
       nd=nd+1
      endif
      enddo
10    continue

c --- Check
      nd=0
      do i=1,n
       if(is(i).eq.0) nd=nd+1
      enddo
      write(6,*) 'nd = ',nd

      nd=0
      do i=1,n
      if(is(i).gt.0) then
       nd=nd+1
       wx(nd)=x(i)
       wy(nd)=y(i)
       wz(nd)=z(i)
      endif
      enddo
      n=nd
      if(idis.eq.1) write(6,*) 'Number of atom: ',n

      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=1
      enddo

      end
c
      subroutine writedata
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'writedata is started.'

c --- Write data.file for LAMMPS
      if(idis.eq.1) write(6,*) 'Writing data.file.'
      if(idis.eq.1) write(6,*) n,'atoms'
      if(idis.eq.1) write(6,*) nty,'atom types'
      if(idis.eq.1) write(6,*) xmin,xmax,'xlo xhi'
      if(idis.eq.1) write(6,*) ymin,ymax,'ylo yhi'
      if(idis.eq.1) write(6,*) zmin,zmax,'zlo zhi'

      open(10,file='data.file',status='unknown')
      write(10,*) '# Position data'
      write(10,*)
      write(10,*) n,'atoms'
      write(10,*) nty,'atom types'
      write(10,*) xmin,xmax,'xlo xhi'
      write(10,*) ymin,ymax,'ylo yhi'
      write(10,*) zmin,zmax,'zlo zhi'
      write(10,*)
      write(10,*) 'Atoms'
      write(10,*)
      do i=1,n
       write(10,10010) i,is(i),x(i),y(i),z(i)
c       write(6,*) i,is(i),x(i),y(i),z(i)
      enddo
      close(10)
10010 format(i8,i6,3f15.7)

      end
c
      subroutine syssize
      use parameters
      use variables
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'syssize is started.'

c --- system size
      x0=x(1)
      x1=x(1)
      y0=y(1)
      y1=y(1)
      z0=z(1)
      z1=z(1)
      do i=1,n
         if(x(i).ge.x1) x1=x(i)
         if(x(i).le.x0) x0=x(i)
         if(y(i).ge.y1) y1=y(i)
         if(y(i).le.y0) y0=y(i)
         if(z(i).ge.z1) z1=z(i)
         if(z(i).le.z0) z0=z(i)
      enddo

      if(idis.eq.1) then
         write(6,*) 'System size'
         write(6,100) 'xmax,xmin : ',x1,x0
         write(6,100) 'ymax,ymin : ',y1,y0
         write(6,100) 'zmax,zmin : ',z1,z0
      endif
100   format(a13,2e15.7)

      end

      SUBROUTINE URAND2(N, X, IR)
************************************************************************
* UNIFORM RANDOM NUMBER GENERATOR (MULTIPLICATIVE CONGRUENTIAL METHOD) *
*    FAST BUT NOT COMPLETELY PORTABLE.  INTEGERS SHOULD BE 32 BIT LONG.*
*    OVERFLOW IN INTERGER ARITHMETIC SHOULD NOT BE SENSED.             *
*    BITWISE AND FUNCTION 'IAND(IA,IB)' SHOULD BE SUPPORTED.           *
* PARAMETERS                                                           *
*   (1) N      (I) THE NUMBER OF RANDOM NUMBERS TO BE GENERATED        *
*                  (INPUT)                                             *
*   (2) X      (D) UNIFORM RANDOM NUMBERS (OUTPUT)                     *
*   (3) IR     (I) THE INITIAL SEED  (INPUT)                           *
*                  THE SEED FOR THE NEXT CALL (OUTPUT)                 *
* COPYRIGHT: Y. OYANAGI, JUNE 30, 1989  V.1                            *
************************************************************************
      implicit doubleprecision (a-h,o-z)
      dimension X(N)
      PARAMETER (LAMBDA = 48828125, MASK=2**30+(2**30-1) )
      PARAMETER (aINVM = 0.5D0 ** 31)
*PAREMETER CHECK
      IF( N .LE. 0) THEN
       WRITE(6,*) '(SUBR.URAND2) PARAMETER ERROR. N = ', N
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      END IF
      IF( MOD(IR, 2) .NE. 1) THEN
       WRITE(6,*) '(SUBR.URAND2) PARAMETER ERROR. IR = ', IR, 
     1            ' IS EVEN.'
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      END IF
*MAIN LOOP
      DO 10 I = 1, N
       IR = IAND(LAMBDA * IR, MASK)
       X(I) = IR * aINVM
   10 CONTINUE
      RETURN
      END
