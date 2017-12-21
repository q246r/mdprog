c 
c Post process tools for LAMMSP
c - Least squares method
c
      module variables
       integer,dimension(:),allocatable :: id,is
       doubleprecision,dimension(:),allocatable ::
     &  x,y,z,s1,s2,s3,p
       integer :: n,nty
       doubleprecision :: xmax,xmin,ymax,ymin,zmax,zmin
      end
c
      module parameters
       integer :: idis,ndis,nrot,js
       doubleprecision :: x0,x1,y0,y1,z0,z1,dw
       integer :: nx,ny,nz,nh,nv
       doubleprecision :: ac,rc
       doubleprecision :: c11,c12,c44,rame1,rame2
       doubleprecision :: ym,pr
      end
c
      module data
       integer :: ntot
       real,dimension(:),allocatable :: x,y
       real :: a,b
      end
c
c --- xc: x-coordinate
c --- yc: y-coordinate
c --- zc: physical value
c
      module contour
       doubleprecision,dimension(:,:),allocatable ::
     &  xc,yc,zc
       integer :: ncx,ncy
      end
c
      module tool
c --- bar to GPa
       real :: b2p=0.0001
c --- Angstrom to nm
       real :: a2n=0.1
c --- Angstrom^2 to nm^2
       real :: a2n2=0.01
c --- ps to ns
       real :: p2n=0.001
c --- eV to J, 1 eV = 1.60219*10^{-19} J
       real :: ev2j=1.60219d-19
      end
c
      program main
      use parameters

      idis=1

c --- Select calculation
      ical=7
      if(ical.eq.0) write(6,*) '0: time average is selected.'
      if(ical.eq.1) write(6,*) '1: msd is selected.'
      if(ical.eq.2) write(6,*) '2: diffusivity is selected.'
      if(ical.eq.3) write(6,*) '3: arrhenius plot is selected.'
      if(ical.eq.4) write(6,*) '4: lattice constant is selected.'
      if(ical.eq.5) write(6,*) '5: dump post is selected.'
      if(ical.eq.6) write(6,*) '6: msd files are combined.'
      if(ical.eq.7) write(6,*) '7: thermo is selected.'
      if(ical.eq.9) write(6,*) '9: elastic constant is selected.'
      if(ical.eq.11) write(6,*) '11: test for contour map is selected.'
      if(ical.eq.12) write(6,*) 
     &   '12: pressure near dislocation is selected.'
      if(ical.eq.13) write(6,*) 
     &   '13: pressure near dislocation dipole is selected.'
      if(ical.eq.14) write(6,*) '14: stress calculation is selected.'
      if(ical.eq.15) write(6,*) '15: concentration calc. is selected.'
      if(ical.eq.16) write(6,*) '16: stacking fault energy is selected.'
      if(ical.eq.17) write(6,*) 
     &   '17: load-displacement curve is selected.'
      if(ical.eq.18) write(6,*) '18: Hertz solution is selected.'
      if(ical.eq.19) write(6,*) '19: Slice a system is selected.'
      if(ical.eq.20) write(6,*) '20: unloading curve curve is selected.'
      if(ical.eq.21) write(6,*) '21: VRH average is selected.'
      if(ical.eq.0) call timeave
      if(ical.eq.1) call msdcalc
      if(ical.eq.2) call difcalc
      if(ical.eq.3) call arrhenius
      if(ical.eq.4) call lattice
      if(ical.eq.5) call dump
      if(ical.eq.6) call msdcomb
      if(ical.eq.7) call thermo
      if(ical.eq.9) call elastic
      if(ical.eq.11) call testcntr
      if(ical.eq.12) call press
      if(ical.eq.13) call press2
      if(ical.eq.14) call stress
      if(ical.eq.15) call concen
      if(ical.eq.16) call stack
      if(ical.eq.17) call ldcurve
      if(ical.eq.18) call hertz
      if(ical.eq.19) call slice
      if(ical.eq.20) call unload
      if(ical.eq.21) call vrhave

      end
c
      subroutine thermo
      use tool
      character (len=80) :: com
      integer,dimension(:),allocatable :: m
      real,dimension(:),allocatable :: t,p,v,e
      real,dimension(:),allocatable :: s1,s2,s3,s4,s5,s6

      write(6,*) 'thermo is started.'

c --- Select isw
c ---  1: thermo
c ---  2: stress
c ---  3: thermo and stress
      isw=3

      nx=10000
      allocate(m(nx),t(nx),p(nx),v(nx),e(nx))
      allocate(s1(nx),s2(nx),s3(nx),s4(nx),s5(nx),s6(nx))

      if(isw.eq.1.or.isw.eq.3) then
c --- thermo info
      write(6,*) 'thermo info is selected.'

c --- delta t
c ---  dt=1(fs)
      dt=1.0d-6
c --- # of lattice
      na=10

c --- Read data
      open(10,file='thermo.d',status='old')
      read(10,100) com
      write(6,100) com
      read(10,100) com
      write(6,100) com
100   format(a80)
      n=1
      do i=1,nx
       read(10,*,end=99) m(n),t(n),p(n),v(n),e(n)
       n=n+1
      enddo
99    continue
      close(10)
      n=n-1
      write(6,*) '# of data:',n

c --- Unit tranfer
c --- bar to GPa
c --- A^3 to nm^3
      c3=a2n*a2n*a2n
      do i=1,n
       p(i)=p(i)*b2p
       v(i)=v(i)*c3
      enddo

c --- Time average (all time period)
      write(6,*) 'Time average (all time period)'

      ist=1
      ien=n
      ttot=0.0
      ptot=0.0
      vtot=0.0
      etot=0.0
      nd=0
      do i=ist,ien
       ttot=ttot+t(i)
       ptot=ptot+p(i)
       vtot=vtot+v(i)
       etot=etot+e(i)
       nd=nd+1
      enddo
      write(6,*) '# of data:',nd

      tave=ttot/real(nd)
      pave=ptot/real(nd)
      vave=vtot/real(nd)
      eave=etot/real(nd)
      ac=vave**(1.0/3.0)/real(na)
      write(6,*) 'tave =',tave,'[K]'
      write(6,*) 'pave =',pave,'[GPa]'
      write(6,*) 'vave =',vave,'[nm^3]'
      write(6,*) 'eave =',eave,'[eV]'
c      write(6,*) 'ac =',ac,'[nm]'

c --- Time average of last 20 % time period
      write(6,*) 'Time average (last 20 % time period)'

      ist=n*0.8+1
      ien=n
      ttot=0.0
      ptot=0.0
      vtot=0.0
      etot=0.0
      nd=0
      do i=ist,ien
       ttot=ttot+t(i)
       ptot=ptot+p(i)
       vtot=vtot+v(i)
       etot=etot+e(i)
       nd=nd+1
      enddo
      write(6,*) '# of data:',nd

      tave=ttot/real(nd)
      pave=ptot/real(nd)
      vave=vtot/real(nd)
      eave=etot/real(nd)
      ac=vave**(1.0/3.0)/real(na)
      write(6,*) 'From',m(ist),'To',m(ien),'time steps:'
      write(6,*) 'tave =',tave,'[K]'
      write(6,*) 'pave =',pave,'[GPa]'
      write(6,*) 'vave =',vave,'[nm^3]'
      write(6,*) 'eave =',eave,'[eV]'
c      write(6,*) 'ac =',ac,'[nm]'

c --- Output data
      open(10,file='thermo',status='unknown')
      write(10,1000)
      do i=1,n
       write(10,2000) m(i)*dt,t(i),p(i),v(i),e(i)
      enddo
      close(10)
1000  format('# time(ns), temp(K), press(GPa), vol(nm^3), pe(eV)')
2000  format(5e15.7)

      endif

      if(isw.eq.2.or.isw.eq.3) then
c --- stress info
      write(6,*) 'stress info is selected.'

c --- delta t
c ---  dt=1(fs)
      dt=1.0d-6

c --- Read data
      open(10,file='stress.d',status='old')
      read(10,200) com
      write(6,200) com
      read(10,200) com
      write(6,200) com
200   format(a80)
      n=1
      do i=1,nx
       read(10,*,end=98) m(n),s1(n),s2(n),s3(n),s4(n),s5(n),s6(n)
       n=n+1
      enddo
98    continue
      close(10)
      n=n-1
      write(6,*) '# of data:',n

c --- Unit tranfer
c --- bar to GPa
      do i=1,n
       s1(i)=s1(i)*b2p
       s2(i)=s2(i)*b2p
       s3(i)=s3(i)*b2p
       s4(i)=s4(i)*b2p
       s5(i)=s5(i)*b2p
       s6(i)=s6(i)*b2p
      enddo

c --- Time average (all time period)
      write(6,*) 'Time average (all time period)'

      ist=1
      ien=n
      s1tot=0.0
      s2tot=0.0
      s3tot=0.0
      s4tot=0.0
      s5tot=0.0
      s6tot=0.0
      nd=0
      do i=ist,ien
       s1tot=s1tot+s1(i)
       s2tot=s2tot+s2(i)
       s3tot=s3tot+s3(i)
       s4tot=s4tot+s4(i)
       s5tot=s5tot+s5(i)
       s6tot=s6tot+s6(i)
       nd=nd+1
      enddo
      write(6,*) '# of data:',nd

      s1ave=s1tot/real(nd)
      s2ave=s2tot/real(nd)
      s3ave=s3tot/real(nd)
      s4ave=s4tot/real(nd)
      s5ave=s5tot/real(nd)
      s6ave=s6tot/real(nd)
      write(6,*) 's1ave =',s1ave,'[GPa]'
      write(6,*) 's2ave =',s2ave,'[GPa]'
      write(6,*) 's3ave =',s3ave,'[GPa]'
      write(6,*) 's4ave =',s4ave,'[GPa]'
      write(6,*) 's5ave =',s5ave,'[GPa]'
      write(6,*) 's6ave =',s6ave,'[GPa]'

c --- Time average of last 20 % time period
      write(6,*) 'Time average (last 20 % time period)'

      ist=n*0.8+1
      ien=n
      ttot=0.0
      s1tot=0.0
      s2tot=0.0
      s3tot=0.0
      s4tot=0.0
      s5tot=0.0
      s6tot=0.0
      nd=0
      do i=ist,ien
       s1tot=s1tot+s1(i)
       s2tot=s2tot+s2(i)
       s3tot=s3tot+s3(i)
       s4tot=s4tot+s4(i)
       s5tot=s5tot+s5(i)
       s6tot=s6tot+s6(i)
       nd=nd+1
      enddo
      write(6,*) '# of data:',nd

      s1ave=s1tot/real(nd)
      s2ave=s2tot/real(nd)
      s3ave=s3tot/real(nd)
      s4ave=s4tot/real(nd)
      s5ave=s5tot/real(nd)
      s6ave=s6tot/real(nd)
      write(6,*) 's1ave =',s1ave,'[GPa]'
      write(6,*) 's2ave =',s2ave,'[GPa]'
      write(6,*) 's3ave =',s3ave,'[GPa]'
      write(6,*) 's4ave =',s4ave,'[GPa]'
      write(6,*) 's5ave =',s5ave,'[GPa]'
      write(6,*) 's6ave =',s6ave,'[GPa]'

c --- Output data
      open(10,file='stress',status='unknown')
      write(10,3000)
      do i=1,n
       write(10,4000) m(i)*dt,s1(i),s2(i),s3(i),s4(i),s5(i),s6(i)
      enddo
      close(10)
3000  format('# time(ns), stress1-6(GPa)')
4000  format(7e15.7)

      endif

      end
c
      subroutine timeave
      use tool
      integer,dimension(:),allocatable :: m
      real,dimension(:),allocatable :: c,d,e,f,g,h
      real,dimension(:),allocatable :: s4,s5,s6
      real,dimension(:),allocatable :: s4a,s5a,s6a
      real,dimension(:),allocatable :: s4b,s5b,s6b
      character (len=80) :: com

      nx=100000
      allocate(c(nx),d(nx),e(nx),f(nx),g(nx),h(nx),m(nx))
      allocate(s4(nx),s5(nx),s6(nx))
      allocate(s4a(nx),s5a(nx),s6a(nx))
      allocate(s4b(nx),s5b(nx),s6b(nx))

c --- Without hydrogen
c      dt=1.0e-3
c --- With hydrogen
      dt=1.0e-4

c --- Read data
      open(10,file='thermo.profile',status='old')
      read(10,100) com
      write(6,100) com
      read(10,100) com
      write(6,100) com
100   format(a80)
      n=1
      do i=1,nx
c       read(10,*,end=99) m(n),c(n),d(n)
       read(10,*,end=99) m(n),c(n),d(n),s4(n),s5(n),s6(n)
c       write(6,*) m(i),c(i),d(i)
       n=n+1
      enddo
99    continue
      close(10)
      n=n-1
      write(6,*) '# of data:',n

c --- Data shift
c --- No shift
      nsh=0
      m0=0
c --- Shift
c      nsh=n/11
c      m0=m(nsh)
c      n=n-nsh

      write(6,*) 'Data shift:',nsh

      do i=1,n
       m(i)=m(i+nsh)-m0
       c(i)=c(i+nsh)
       d(i)=d(i+nsh)
       s4(i)=s4(i+nsh)
       s5(i)=s5(i+nsh)
       s6(i)=s6(i+nsh)
      enddo

c --- Calc time average
      e=0.0
      f=0.0
      s4a=0.0
      s5a=0.0
      s6a=0.0
      sumc=0.0
      sumd=0.0
      sums4=0.0
      sums5=0.0
      sums6=0.0
      do i=1,n
       sumc=sumc+c(i)
       sumd=sumd+d(i)
       sums4=sums4+s4(i)
       sums5=sums5+s5(i)
       sums6=sums6+s6(i)
       e(i)=sumc/i
       f(i)=sumd/i
       s4a(i)=sums4/i
       s5a(i)=sums5/i
       s6a(i)=sums6/i
      enddo
      write(6,*) 'Time average (all time period)'
      write(6,200) m(n),e(n),f(n)*b2p

c --- Time average of the last 20 % time period
      g=0.0
      h=0.0
      s4b=0.0
      s5b=0.0
      s6b=0.0
      sumc=0.0
      sumd=0.0
      sums4=0.0
      sums5=0.0
      sums6=0.0
      nd=0
      i1=n*0.8
      do i=i1,n
       sumc=sumc+c(i)
       sumd=sumd+d(i)
       sums4=sums4+s4(i)
       sums5=sums5+s5(i)
       sums6=sums6+s6(i)
       nd=nd+1
       g(i)=sumc/nd
       h(i)=sumd/nd
       s4b(i)=sums4/nd
       s5b(i)=sums5/nd
       s6b(i)=sums6/nd
      enddo
      tac=sumc/nd
      tad=sumd/nd
      tas4=sums4/nd
      tas5=sums5/nd
      tas6=sums6/nd
      write(6,*) 'Time average (last 80 % time period)'
      write(6,*) 'From',m(i1),'To',m(n),'time steps:'
      write(6,*) tac,'[K]',tad*b2p,'[GPa]'
c      write(6,*) tas4*b2p,'[GPa]',tas5*b2p,'[GPa]',tas6*b2p,'[GPa]'
      write(6,*) tas4*1.0e-3,'[nm3]'

c --- Write data
      nd=500
      ns=n/nd
      open(20,file='tave',status='unknown')
      write(20,300)
      do i=1,n,ns
       write(20,200) m(i),e(i),f(i)
      enddo
      close(20)
200   format(i10,2e15.7)

      open(30,file='tave0',status='unknown')
      write(30,300)
      do i=1,n,ns
c       write(30,400) m(i)*dt,c(i),d(i)*b2p
c       write(30,400) m(i)*dt*p2n,c(i),d(i)*b2p,
c     &  s4(i)*b2p,s5(i)*b2p,s6(i)*b2p
       write(30,400) m(i)*dt*p2n,c(i),d(i)*b2p,s4(i)*1.0e-3
      enddo
      close(30)
300   format('# timestep ave_temp ave_press ave_s4 ave_s5 ave_s6')
400   format(6e15.7)

      open(40,file='tave1',status='unknown')
      write(40,300)
      do i=1,n,ns
c       write(40,400) m(i)*dt,e(i),f(i)*b2p
c       write(40,400) m(i)*dt*p2n,e(i),f(i)*b2p,
c     &  s4a(i)*b2p,s5a(i)*b2p,s6a(i)*b2p
       write(40,400) m(i)*dt*p2n,e(i),f(i)*b2p,s4a(i)*1.0e-3
      enddo
      close(40)

      open(50,file='tave2',status='unknown')
      write(50,300)
      do i=i1,n,ns
       write(50,400) m(i)*dt,g(i),h(i)*b2p
      enddo
      close(50)

      end
c
      subroutine msdcalc
      use data
      use tool
      integer,dimension(:),allocatable :: m
      real,dimension(:),allocatable :: c,d,e,f
      real,dimension(:),allocatable :: ax,ay,az,bx,by,bz
      character (len=80) :: com

      nx=100000
      allocate(c(nx),d(nx),e(nx),f(nx),m(nx))
      allocate(ax(nx),ay(nx),az(nx),bx(nx),by(nx),bz(nx))

c --- Without hydrogen
c      dt=1.0e-3
c --- With hydrogen
      dt=1.0e-4

c --- Read data
c      open(10,file='msd.profile',status='old')
      open(10,file='msd.d',status='old')
c      open(10,file='msds.profile',status='old')
      read(10,100) com
      write(6,100) com
      read(10,100) com
      write(6,100) com
100   format(a80)
      n=0
      do i=1,nx
       n=n+1
       read(10,*,end=99) m(n),c(n),ax(n),ay(n),az(n)
c       write(6,*) m(i),c(i)
      enddo
99    continue
      close(10)
      n=n-1
      write(6,*) '# of data:',n

      allocate(x(n),y(n))

c --- Unit change
c --- Time step -> psec
c --- A^2 -> nm^2
      do i=1,n
       f(i)=m(i)*dt
       c(i)=c(i)*a2n2
       ax(i)=ax(i)*a2n2
       ay(i)=ay(i)*a2n2
       az(i)=az(i)*a2n2
      enddo

c --- Calc time average
      e=0.0
      sumc=0.0
      sumax=0.0
      sumay=0.0
      sumaz=0.0
      do i=1,n
       sumc=sumc+c(i)
       sumax=sumax+ax(i)
       sumay=sumay+ay(i)
       sumaz=sumaz+az(i)
       d(i)=sumc/i
       bx(i)=sumax/i
       by(i)=sumay/i
       bz(i)=sumaz/i
      enddo
      write(6,*) 'Time average (all time period)'
      write(6,200) m(n),d(n)*a2n2

      ist=0.8*n+1
      ien=n
      write(6,*) 'ist,ien:',ist,ien
c --- D
      nd=1
      do i=ist,ien
       x(nd)=f(i)
       y(nd)=d(i)
c       y(nd)=c(i)
       nd=nd+1
      enddo
      ntot=nd-1
c      write(6,*) '# of data:',ntot

      call least_squares
      write(6,*) 'least_squares has been finished.'
      dc=a*1.0e-6/6.0

c --- Dx
      write(6,*) 'ist,ien:',ist,ien
      nd=1
      do i=ist,ien
       x(nd)=f(i)
       y(nd)=bx(i)
c       y(nd)=c(i)
       nd=nd+1
      enddo
      ntot=nd-1
c      write(6,*) '# of data:',ntot

      call least_squares
      write(6,*) 'least_squares has been finished.'
      dcx=a*1.0e-6/2.0

c --- Dy
      write(6,*) 'ist,ien:',ist,ien
      nd=1
      do i=ist,ien
       x(nd)=f(i)
       y(nd)=by(i)
c       y(nd)=c(i)
       nd=nd+1
      enddo
      ntot=nd-1
c      write(6,*) '# of data:',ntot

      call least_squares
      write(6,*) 'least_squares has been finished.'
      dcy=a*1.0e-6/2.0

c --- Dz
      write(6,*) 'ist,ien:',ist,ien
      nd=1
      do i=ist,ien
       x(nd)=f(i)
       y(nd)=bz(i)
c       y(nd)=c(i)
       nd=nd+1
      enddo
      ntot=nd-1
c      write(6,*) '# of data:',ntot

      call least_squares
      write(6,*) 'least_squares has been finished.'
      dcz=a*1.0e-6/2.0

      write(6,*) 'Diffusion coefficient (m^2/s)'
      write(6,*) 'D :',dc
      write(6,*) 'Dx:',dcx
      write(6,*) 'Dy:',dcy
      write(6,*) 'Dz:',dcz
      write(6,*) '(Time steps:',m(ist),m(ien),')'

c --- Write data
      nd=500
      ns=n/nd
      open(20,file='msd',status='unknown')
      write(20,300)
      do i=1,n,ns
       write(20,200) m(i),e(i)
      enddo
      close(20)
200   format(i10,2e15.7)
300   format('# timestep msd')

      open(30,file='msd0',status='unknown')
      write(30,300)
      do i=1,n,ns
c       write(30,400) f(i),c(i)
       write(30,400) f(i)*p2n,c(i),ax(i),ay(i),az(i)
      enddo
      close(30)
400   format(5e15.7)
500   format('# time ave_msd')

      open(40,file='msd1',status='unknown')
      write(40,300)
      do i=1,n,ns
c       write(40,400) f(i),d(i)
       write(40,400) f(i)*p2n,d(i),bx(i),by(i),bz(i)
      enddo
      close(40)

      end
c
      subroutine difcalc
      use data
      real,dimension(:),allocatable :: c,d
      character (len=80) :: com

      write(6,*) 'Start diffusicity.'

      nx=100
      allocate(c(nx),d(nx))

      write(6,*) 'Reading data file ...'
c      open(10,file='/cygdrive/c/work/121015-dif.txt',status='old')
      open(10,file='dif',status='old')
      read(10,*) com
      write(6,*) com
c      write(6,*) ' Temperature(K) Diffusivity(m^2/s)'
      ntot=1
      do i=1,nx
       read(10,*,end=99) c(ntot),dumy,d(ntot)
       write(6,10010) c(i),d(i)
       ntot=ntot+1
      enddo
99    continue
      close(10)
      ntot=ntot-1
      write(6,*) '# of data:',ntot
      allocate(x(ntot),y(ntot))

c      write(6,*) ' 1/T            log(D)'
      do i=1,ntot
       x(i)=1.d0/c(i)
       y(i)=log(d(i))
c       write(6,10010) x(i),y(i)
      enddo
10010 format(2e15.7)

      call least_squares
      write(6,*) 'least_squares has been finished.'

c --- Bolzmann constant, J/K
      akb=1.380658d-23
c --- Convert energy unit
c --- 1 eV = 1.60219*10^{-19} J
      engj=1.60219d-19
      akb=akb/engj
      write(6,*) 'Bolzmann constant (eV/K):',akb

      ea=-a*akb
      dd=exp(b)
      write(6,*) 'activation energy (eV):'
      write(6,*) ea
      write(6,*) 'pre-exponential factor (m^2/s):'
      write(6,*) dd

      write(6,*) 'Complete diffusivity.'

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
c
      subroutine arrhenius

      write(6,*) 'Start Arrhenius plot.'

c --- Bolzmann constant, J/K
      akb=1.380658e-23
c --- Convert energy unit
c --- 1 eV = 1.60219*10^{-19} J
      engj=1.60219e-19
      akb=akb/engj
c      write(6,*) 'Bolzmann constant (eV/K):',akb

c --- Temperature
      tt=300.0
c      tt=800.0
      ea0=0.230
c      dd0=1.0e-7
      dd0=2.9e-7
c --- Experiment
c --- Pd, 78-Volkl
c      ea0=0.23
c      dd0=2.9e-7
c --- PdH, 75-Seymour
c      ea0=0.228
c      dd0=9.0e-8
c --- Wicke
c --- Activation energy
c      ea0=0.24
c --- Preexponential factor
c      dd0=5.3e-7
c --- 92-Li
c --- Activation energy
c      ea0=0.245
c --- Preexponential factor
c      dd0=4.8e-7
c
c --- Ni (220 - 330 K)
c      ea0=0.40e0
c      dd0=1.8e-7
c --- Ni (385 - 620 K)
c      ea0=0.41e0
c      dd0=6.7e-7
c --- Ni (620 - 1600 K)
c      ea0=0.42e0
c      dd0=6.9e-7
c --- Diffusion in solids
c --- Ni (T < 627 K)
c      ea0=0.41e0
c      dd0=4.76e-7
c --- Ni (T > 627 K)
c      ea0=0.42e0
c      dd0=6.87e-7

      di0=dd0*exp(-ea0/tt/akb) 
      write(6,*) 'Ea, D0 =', ea0,dd0
      write(6,*) 'T (K), D (m^2/s) =',tt,di0

      write(6,*) 'Complete Arrhenius plot.'

      end
c
      subroutine lattice
      use data

      ntot=6
      allocate(x(ntot),y(ntot))

      ac=3.885

      x(1)=1000.0
      x(2)=800.0
      x(3)=600.0
      x(4)=500.0
      x(5)=400.0
      x(6)=300.0

      y(1)=1.0139*ac
      y(2)=1.0106*ac
      y(3)=1.0076*ac
      y(4)=1.0062*ac
      y(5)=1.0049*ac
      y(6)=1.0037*ac

      call least_squares
      write(6,*) 'least_squares has been finished.'

      t=350.0
      aa=a*t+b
      write(6,*) 'T,ac,rate:',t,aa,(aa-ac)/ac

      write(6,*) 'Coefficient of thermal expansion:'
      write(6,*) a/ac, '[/K]'

      c=1.1e-5
      do i=1,6
       write(6,*) x(i),y(i),a*x(i)+b,ac*(1+c*x(i))
      enddo

      end
c
      subroutine dump
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: a,b,c
      integer,dimension(:),allocatable :: ia

c --- Read LAMMPS dump file
      call readdump
      if(idis.eq.1) write(6,*) 'readdump has been finished.'
c      nty=2

      allocate(a(n),b(n),c(n),ia(n))

      m=0
      do i=1,n
       if(is(i).eq.1) then
        m=m+1
        a(m)=x(i)
        b(m)=y(i)
        c(m)=z(i)
        ia(m)=is(i)
       endif
      enddo

      do i=1,m
       x(i)=a(i)
       y(i)=b(i)
       z(i)=c(i)
       is(i)=ia(i)
      enddo

      n=m
      if(idis.eq.1) write(6,*) 'n =',n
      nty=1

c --- Write LAMMPS data file
      call writedata
      if(idis.eq.1) write(6,*) 'writedata has been finished.'

      end
c
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
      allocate(x(n),y(n),z(n),id(n),is(n))
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
c       read(10,*) is(i),x(i),y(i),z(i)
       read(10,*) id(i),is(i),x(i),y(i),z(i)
      enddo

      close(10)
100   format(a80)

      end
c
      subroutine writedump
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      character (len=80) :: com

      open(10,file='res1',status='unknown')

c      write(10,*) 'ITEM: TIMESTEP'
      write(10,1000)
      write(10,*) 0
c      write(10,*) 'ITEM: NUMBER OF ATOMS'
      write(10,2000)
      write(10,*) n
      if(idis.eq.1) write(6,*) 'n =',n
c      write(10,*) 'ITEM: BOX BOUNDS pp pp ss'
      write(10,3000)
      write(10,100) xmin,xmax
      write(10,100) ymin,ymax
      write(10,100) zmin,zmax
c      write(10,*) 'ITEM: ATOMS id type xs ys zs'
c      write(10,*) 'ITEM: ATOMS id type x y' 
      write(10,4000) 
      do i=1,n
c       read(10,*) is(i),x(i),y(i),z(i)
c       write(10,200) id(i),is(i),x(i),y(i),z(i)
       write(10,200) i,is(i),x(i),y(i),z(i)
      enddo
100   format(2e15.7)
200   format(i8,i6,3e15.7)
1000  format('ITEM: TIMESTEP')
2000  format('ITEM: NUMBER OF ATOMS')
3000  format('ITEM: BOX BOUNDS pp pp ss')
4000  format('ITEM: ATOMS id type x y z')

      close(10)

      end
c
      subroutine msdcomb
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:,:),allocatable :: t0,a0,b0,c0,d0
      integer,dimension(:),allocatable :: m
      doubleprecision,dimension(:),allocatable :: t,a,b,c,d
      character(len=80),dimension(:),allocatable :: fn
      character(len=80) :: com

      nd=2
      allocate(fn(nd),m(0:nd))
      mmx=1000
      allocate(t0(mmx,0:nd),a0(mmx,0:nd),b0(mmx,0:nd),c0(mmx,0:nd),
     & d0(mmx,0:nd))
      nmx=10000
      allocate(t(nmx),a(nmx),b(nmx),c(nmx),d(nmx))

c --- File name
      fn(1)='/cygdrive/c/work/130223-msd-300K.txt'
      fn(2)='/cygdrive/c/work/130305-msd-300K.txt'

c --- Read msd files
      do k=1,nd
      m(k)=0
      open(20,file=fn(k),status='old')
      read(20,*) com
      j=1
      do i=1,mmx
       read(20,*,end=99) t0(j,k),d0(j,k),a0(j,k),b0(j,k),c0(j,k)
c       write(6,*) t0(j,k),d0(j,k)
       j=j+1
      enddo
99    continue
      m(k)=j-1
      close(20)
      enddo

      do i=1,nd
       write(6,*) '# of data:',m(i)
       write(6,*) fn(i)
      enddo

      i=1
      do j=1,nd
      do k=1,m(j)
       t(i)=t0(k,j)+t0(m(j-1),j-1)
       d(i)=d0(k,j)+d0(m(j-1),j-1)
       a(i)=a0(k,j)+a0(m(j-1),j-1)
       b(i)=b0(k,j)+b0(m(j-1),j-1)
       c(i)=c0(k,j)+c0(m(j-1),j-1)
       i=i+1
      enddo
      enddo
      n=i-1
      write(6,*) '# of data:',n

c --- Write combined msd file
      open(10,file='msd1',status='unknown')
      write(10,100)
      do i=1,n
c       write(10,200) t(i),d(i)
       write(10,200) t(i),d(i),a(i),b(i),c(i)
      enddo
      close(10)
100   format('# timestep msd msdx msdy msdz')
200   format(5e15.7)

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
c       write(10,10010) i,is(i),x(i),y(i),z(i)
c       write(6,*) i,is(i),x(i),y(i),z(i)
       write(10,10010) id(i),is(i),x(i),y(i),z(i)
      enddo
      close(10)
10010 format(i8,i6,3f15.7)

      end
c
      subroutine elastic
      use parameters

      if(idis.eq.1) write(6,*) 'elastic is selected.'

c --- Pb by Zhou
c      c11=246.65
c      c12=200.47
c      c44=55.67
c --- PdH by Zhou
      c11=241.7
      c12=190.3
      c44=25.5
c      ac=3.885
c --- Pb by Exp.
c      c11=234.0
c      c12=176.0
c      c44=71.0
c --- Al by Mishin
c      c11=111.9
c      c12=61.2
c      c44=31.8
c --- Cu_u3
c      c11=167.3
c      c12=124.2
c      c44=76.4
c --- 3C-SiC by Vashishta
c      c11=390.0
c      c12=142.6
c      c44=191.0
c --- 3C-SiC by Erhart
c      c11=382.0
c      c12=145.0
c      c44=240.0

c --- 1: Thesis by Prof. Shimokawa
c --- 2: 07-Vashishta-JAP
c --- 3: 16-Chavoshi-MSEA
      js=1

      if(js.eq.1) then

c --- Thesis by Prof. Shimokawa
      if(idis.eq.1) write(6,*) 'Shimokawa is selected.'
      b=(c11+c12*2.0)/3.0
      g1=(c11-c12)*0.5
      g2=c44
      a=g2/g1
      ramdav=(c11+4.0*c12-2.0*c44)*0.2
      umv=(c11-c12+3.0*c44)*0.2
      smv=(2.0*g1+3.0*g2)*0.2
      smr=5.0*g1*g2/(3.0*g1+2.0*g2)
      ymv=9.0*b*smv/(3.0*b+smv)
      ymr=9.0*b*smr/(3.0*b+smr)
      prv=ymv/smv*0.5-1.0
      prr=ymr/smr*0.5-1.0

      write(6,*) 'C11 (GPa) =',c11
      write(6,*) 'C12 (GPa) =',c12
      write(6,*) 'C44 (GPa) =',c44
      write(6,*)
      write(6,*) 'B (GPa) =',b
      write(6,*) 'G1 (GPa) =',g1
      write(6,*) 'G2 (GPa) =',g2
      write(6,*) 'A =',a
      write(6,*) 'ramdav =',ramdav
      write(6,*) 'ramdar =',b-2.0*smr/3.0
      write(6,*) 'miuv =',prv
      write(6,*) 'miur =',smr
      write(6,*) 'GV (GPa) =',smv
      write(6,*) 'GR (GPa) =',smr
      write(6,*) 'EV (GPa) =',ymv
      write(6,*) 'ER (GPa) =',ymr
      write(6,*) 'prv =',prv
      write(6,*) 'prr =',prr

      rame1=ramdav
      rame2=umv
      write(6,*) 'Lame constants:',rame1,rame2 

      elseif(js.eq.2) then

c --- 07-Vashishta-JAP
      if(idis.eq.1) write(6,*) 'Vashishta is selected.'
      ym=(c11-c12)*(c11+2.0*c12)/(c11+c12)
      pr=c12/(c11+c12)
      sm=ym/(1.0+pr)*0.5
      bm=ym/(1.0-2.0*pr)/3.0

      write(6,*) 'C11 (GPa) =',c11
      write(6,*) 'C12 (GPa) =',c12
      write(6,*) 'C44 (GPa) =',c44
      write(6,*)
      write(6,*) 'Young modulus (GPa) =',ym
      write(6,*) 'Poisson ratio (GPa) =',pr
      write(6,*) 'Shear modulus (GPa) =',sm
      write(6,*) 'Bulk modulus (GPa) =',bm

      rame1=ym*pr/(1.0+pr)/(1.0-2.0*pr)
      rame2=sm
      write(6,*) 'Lame constants:',rame1,rame2 

      elseif(js.eq.3) then

c --- 16-Chavoshi-MSEA
      if(idis.eq.1) write(6,*) 'Chavoshi is selected.'

      a=2.0*c44/(c11-c12)
      h=2.0*c44+c12-c11
      b=(c11+2.0*c12)/3.0
      ym100=c11-2.0*c12*c12/(c11+c12)
      ym110=4.0*(c11*c11+c12*c11-2.0*c12*c12)*c44/(2.0*c44*c11+c11*c11
     & +c12*c11-2.0*c12*c12)
      ym111=3.0*c44*(c11+2.0*c12)/(c11+2.0*c12+c44)
      smv=c44-h*0.2
      prv=(c12-h*0.2)*0.5/(c12+c44-h*0.4)
      ymv=2.0*smv*(1.0+prv)

      write(6,*) 'C11 (GPa) =',c11
      write(6,*) 'C12 (GPa) =',c12
      write(6,*) 'C44 (GPa) =',c44
      write(6,*)
      write(6,*) 'Anisotroy ratio (A) =',a
      write(6,*) 'Anisotropy factor (H) (GPa) =',h
      write(6,*) 'Bulk modulus (B) (GPa) = ',b
      write(6,*) 'Youngs modulus (E_100) (GPa) =',ym100
      write(6,*) 'Youngs modulus (E_110) (GPa) =',ym110
      write(6,*) 'Youngs modulus (E_111) (GPa) =',ym111
      write(6,*) 'Voigt shear modulus (GPa) = ',smv
      write(6,*) 'Voigt Poissons ratio = ',prv
      write(6,*) 'Voigt Youngs modulus (GPa) = ',ymv

      endif

      end
c
      subroutine vrhave
      use parameters
      implicit doubleprecision (a-h,o-z)

c --- 11-Man-JE     
c --- Cu
c      c11=169.05d0
c      c12=121.93d0
c      c44=151.00d0
c --- Fe
c      c11=237d0
c      c12=141d0
c      c44=231d0
c --- Pd 08-Zhou using LAMMPS
c      c11=246.7d0
c      c12=200.5d0
c      c44=55.7d0
      c11=182.1d0
      c12=152.7d0
      c44=26.4d0

      if(idis.eq.1) then
       write(6,*) 'C11 (GPa) =',c11
       write(6,*) 'C12 (GPa) =',c12
       write(6,*) 'C44 (GPa) =',c44
      endif

c --- Job selecter
      js=1

      if(js.eq.0) then

c --- 11-Man-JE     
c      c44=c44*2.0d0
c --- Voigt
      av=0.2d0*(c11+4.0d0*c12-c44)
      bv=0.2d0*(c11-c12+1.5d0*c44)
      ymv=bv*(3.0d0*av+2.0d0*bv)/(av+bv)
      prv=0.5d0*av/(av+bv)
      bmv=(3.0d0*av+2.0d0*bv)/3.0d0
c --- Reuss
      d=c11*c11+c11*c12-2.0d0*c12*c12
      s11=(c11+c12)/d
      s12=-c12/d
      s44=1.0d0/c44
      as=0.2d0*(s11+4.0d0*s12-s44)
      bs=0.2d0*(s11-s12+1.5d0*s44)
      ar=-0.5d0*as/bs/(2.0d0*bs+3.0d0*as)
      br=0.25d0/bs
      ymr=br*(3.0d0*ar+2.0d0*br)/(ar+br)
      prr=0.5d0*ar/(ar+br)
      bmr=(3.0d0*ar+2.0d0*br)/3.0d0
c --- Hill
      ah=0.5d0*(av+ar)
      bh=0.5d0*(bv+br)
      ymh=bh*(3.0d0*ah+2.0d0*bh)/(ah+bh)
      prh=0.5d0*ah/(ah+bh)
      bmh=(3.0d0*ah+2.0d0*bh)/3.0d0

      elseif(js.eq.1) then

c --- Computational Quantum Mechanics for Materials Engineers
      bm=(c11+2.0d0*c12)/3.0d0
c --- Voigt
      bmv=bm
      bv=(c11-c12+3.0d0*c44)/5.0d0
      ymv=9.0d0*bmv*bv/(3.0d0*bmv+bv)
      prv=(3.0d0*bmv-2.0d0*bv)/(3.0*bmv+bv)*0.5d0
      av=bmv-2.0d0*bv/3.0d0
c --- Reuss
      bmr=bm
      br=(5.0d0*(c11-c12))*c44/(4.0d0*c44+3.0d0*(c11-c12))
      ymr=9.0d0*bmr*br/(3.0d0*bmr+br)
      prr=(3.0d0*bmr-2.0d0*br)/(3.0*bmr+br)*0.5d0
      ar=bmr-2.0d0*br/3.0d0
c --- Hill
      bmh=bm
      bh=(bv+br)*0.5d0
      ymh=9.0d0*bmh*bh/(3.0d0*bmh+bh)
      prh=(3.0d0*bmh-2.0d0*bh)/(3.0*bmh+bh)*0.5d0
      ah=bmh-2.0d0*bh/3.0d0

      endif

      if(idis.eq.1) then
       write(6,*)
       write(6,*) 'Voigt average:'
       write(6,*) 'Lames constants =',av,bv 
       write(6,*) 'Youngs modulus =',ymv
       write(6,*) 'Shear modulus =',bv 
       write(6,*) 'Poissons ratio =',prv 
       write(6,*) 'Bulk modulus =',bmv 
       write(6,*)
       write(6,*) 'Reuss average:'
       write(6,*) 'Lames constants =',ar,br 
       write(6,*) 'Youngs modulus =',ymr
       write(6,*) 'Shear modulus =',br 
       write(6,*) 'Poissons ratio =',prr 
       write(6,*) 'Bulk modulus =',bmr 
       write(6,*)
       write(6,*) 'Hill average:'
       write(6,*) 'Lames constants =',ah,bh 
       write(6,*) 'Youngs modulus =',ymh
       write(6,*) 'Shear modulus =',bh 
       write(6,*) 'Poissons ratio =',prh 
       write(6,*) 'Bulk modulus =',bmh 
      endif

      end
c
      subroutine testcntr
      use parameters
      use contour
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'testcntr is started.'

      ncx=10
      ncy=10
      allocate(xc(0:ncx,0:ncy),yc(0:ncx,0:ncy),zc(0:ncx,0:ncy))

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=0.0d0
       yc(i,j)=0.0d0
       zc(i,j)=0.0d0
      enddo
      enddo

      pai=acos(-1.0d0)
      pai=1.0d0
      x0=-pai
      x1= pai
      y0=-pai
      y1= pai

      dx=(x1-x0)/ncx
      dy=(y1-y0)/ncy
      
      do i=0,ncx
      do j=0,ncy
       xc(i,j)=x0+i*dx
       yc(i,j)=y0+j*dy
c       zc(i,j)=sin(xc(i,j))*sin(yc(i,j))
       zc(i,j)=exp(-10.0*(xc(i,j)*xc(i,j)+yc(i,j)*yc(i,j)))
      enddo
      enddo

      call writegnuplot
      if(idis.eq.1) write(6,*) 'writegnuplot has been finished.'

      end
c
      subroutine press
      use parameters
      use contour
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'press is started.'

      ncx=51
      ncy=51
      allocate(xc(0:ncx,0:ncy),yc(0:ncx,0:ncy),zc(0:ncx,0:ncy))

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=0.0d0
       yc(i,j)=0.0d0
       zc(i,j)=0.0d0
      enddo
      enddo

      x0=-30.0d0
      x1= 30.0d0
      y0=-30.0d0
      y1= 30.0d0

      dx=(x1-x0)/ncx
      dy=(y1-y0)/ncy

      call elastic
      if(idis.eq.1) write(6,*) 'elastic has been finished.'
      if(idis.eq.1) write(6,*) 'rame1,rame2: ',rame1,rame2
      u=rame1*0.5d0/(rame1+rame2)
      if(idis.eq.1) write(6,*) 'Poisson ratio =',u
      b=ac*sqrt(2.0d0)*0.5d0
      if(idis.eq.1) write(6,*) 'Burgers vector =',b
      pai=acos(-1.0d0)
      
c --- Hydrostatic stress
      om=1.0d0
      ep=1.0d0
      pr=1.0d0/3.0d0
      cc=-rame2*b*(1.0d0+u)*om*ep*pr/pai/(1.0d0-u)

      if(idis.eq.1) write(6,*) 'cc =',cc

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=x0+i*dx
       yc(i,j)=y0+j*dy
       zc(i,j)=cc*yc(i,j)/(xc(i,j)*xc(i,j)+yc(i,j)*yc(i,j))
      enddo
      enddo

      call writegnuplot
      if(idis.eq.1) write(6,*) 'writegnuplot has been finished.'

      end
c
      subroutine press2
      use parameters
      use contour
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'press2 is started.'

      ncx=101
      ncy=101
      allocate(xc(0:ncx,0:ncy),yc(0:ncx,0:ncy),zc(0:ncx,0:ncy))

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=0.0d0
       yc(i,j)=0.0d0
       zc(i,j)=0.0d0
      enddo
      enddo

      x0=-30.0d0
      x1= 30.0d0
      y0=-30.0d0
      y1= 30.0d0

      dx=(x1-x0)/ncx
      dy=(y1-y0)/ncy

      call elastic
      if(idis.eq.1) write(6,*) 'elastic has been finished.'
      if(idis.eq.1) write(6,*) 'rame1,rame2: ',rame1,rame2
      u=rame1*0.5d0/(rame1+rame2)
      if(idis.eq.1) write(6,*) 'Poisson ratio =',u
      b=ac*sqrt(2.0d0)*0.5d0
      if(idis.eq.1) write(6,*) 'Burgers vector =',b
      pai=acos(-1.0d0)

      dx0=-10.0d0
      dy0=10.0d0
      dx1=10.0d0
      dy1=-10.0d0
      
      cc=-rame2*b*(1.0d0+u)/pai/(1.0d0-u)/3.0d0
      if(idis.eq.1) write(6,*) 'cc =',cc

      zmx=100.0d0

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=x0+i*dx
       yc(i,j)=y0+j*dy
       xa=xc(i,j)-dx0
       ya=yc(i,j)-dy0
       za=ya/(xa*xa+ya*ya)
       xb=xc(i,j)-dx1
       yb=yc(i,j)-dy1
       zb=yb/(xb*xb+yb*yb)
c       za=0.0d0
       zc(i,j)=cc*za-cc*zb
       if(zc(i,j).gt.zmx) zc(i,j)=zmx
       if(zc(i,j).lt.-zmx) zc(i,j)=-zmx
      enddo
      enddo

      m=0
      zsum=0.0d0
      do i=0,ncx
      do j=0,ncy
       m=m+1
       zsum=zsum+zc(i,j)
      enddo
      enddo
      if(idis.eq.1) write(6,*) 'm =',m
      if(idis.eq.1) write(6,*) 'zsum =',zsum
      if(idis.eq.1) write(6,*) 'zave =',zsum/m

      call writegnuplot
      if(idis.eq.1) write(6,*) 'writegnuplot has been finished.'

      end
c
      subroutine stress
      use variables
      use parameters
      use contour
      use tool
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: 
     & xw,yw,zw,iw,pw
      integer,dimension(:,:,:),allocatable :: ku

      if(idis.eq.1) write(6,*) 'stress is started.'

c --- Read LAMMPS dump file in which stress data is saved.
      call readdpst
      if(idis.eq.1) write(6,*) 'readdpst has been finished.'

c --- Hydrostatic stress (atom)
c --- Volume
      vol=(xmax-xmin)*(ymax-ymin)*(zmax-zmin)
      va=vol/n

c --- Hydrostatic stress
      do i=1,n
       p(i)=(s1(i)+s2(i)+s3(i))*b2p/3.0d0/va
      enddo

      allocate(xw(n),yw(n),zw(n),iw(n),pw(n))

c --- Remove hydrogen atoms
      m=0
      do i=1,n
      if(is(i).eq.1) then
       m=m+1
       xw(m)=x(i)
       yw(m)=y(i)
       zw(m)=z(i)
       iw(m)=is(i)
       pw(m)=p(i)
      endif
      enddo
      n=m
      if(idis.eq.1) write(6,*) 'n =',n

      do i=1,n
       x(i)=xw(i)
       y(i)=yw(i)
       z(i)=zw(i)
       is(i)=iw(i)
       p(i)=pw(i)
      enddo

c --- Cutting system
c --- Range
c --- No cutting
      rx0=0.0d0
      rx1=1.0d0
      ry0=0.0d0
      ry1=1.0d0
      rz0=0.0d0
      rz1=1.0d0
c --- Figure for CMD 2013
c      rx0=0.0d0
c      rx1=0.63d0
c      ry0=0.0d0
c      ry1=1.0d0
c      rz0=0.0d0
c      rz1=1.0d0
c --- Range of x-direction
      x0=xmin+(xmax-xmin)*rx0
      x1=xmin+(xmax-xmin)*rx1
      if(idis.eq.1) write(6,*) 'x0,x1:',x0,x1
c --- Range of y-direction
      y0=ymin+(ymax-ymin)*ry0
      y1=ymin+(ymax-ymin)*ry1
      if(idis.eq.1) write(6,*) 'y0,y1:',y0,y1
c --- Range of z-direction
      z0=zmin+(zmax-zmin)*rz0
      z1=zmin+(zmax-zmin)*rz1
      if(idis.eq.1) write(6,*) 'z0,z1:',z0,z1

      m=0
      do i=1,n
      if(x(i).ge.x0.and.x(i).le.x1) then
      if(y(i).ge.y0.and.y(i).le.y1) then
      if(z(i).ge.z0.and.z(i).le.z1) then
       m=m+1
       xw(m)=x(i)
       yw(m)=y(i)
       zw(m)=z(i)
       iw(m)=is(i)
       pw(m)=p(i)
      endif
      endif
      endif
      enddo
      n=m
      if(idis.eq.1) write(6,*) 'n =',n

      do i=1,n
       x(i)=xw(i)
       y(i)=yw(i)
       z(i)=zw(i)
       is(i)=iw(i)
       p(i)=pw(i)
      enddo

c --- Max and min of hydrostatic stress
      pmax=p(1)
      pmin=p(1)
      pave=0.0d0
      do i=1,n
       if(p(i).lt.pmin) pmin=p(i)
       if(p(i).gt.pmax) pmax=p(i)
       pave=pave+p(i)
      enddo
      if(idis.eq.1) write(6,*) 'pmin, pmax [GPa] = ',pmin,pmax
      if(idis.eq.1) write(6,*) 'pave [GPa] = ',pave/n

c --- Write LAMMPS dump file for stress data.
      call writedpst
      if(idis.eq.1) write(6,*) 'writedpst has been finished.'

c --- Hydrostatic stress (distribution)
      ncx=21
      ncy=21
      allocate(xc(0:ncx,0:ncy),yc(0:ncx,0:ncy),zc(0:ncx,0:ncy))

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=0.0d0
       yc(i,j)=0.0d0
       zc(i,j)=0.0d0
      enddo
      enddo

c      na=n/ncx/ncy
      na=n
      allocate(ku(0:ncx,0:ncy,0:na))

      do i=0,ncx
      do j=0,ncy
      do k=0,na
       ku(i,j,k)=0
      enddo
      enddo
      enddo

      dx=(xmax-xmin)/ncx
      dy=(ymax-ymin)/ncy

      do i=1,n
       ix=(x(i)-xmin)/dx
       iy=(y(i)-ymin)/dy
       ku(ix,iy,0)=ku(ix,iy,0)+1
       ku(ix,iy,ku(ix,iy,0))=i
      enddo

c      do i=1,ncx
c      do j=1,ncy
c       write(6,*) i,j,ku(i,j,0)
c      enddo
c      enddo

      do i=0,ncx
      do j=0,ncy
      do k=1,ku(i,j,0)
       zc(i,j)=zc(i,j)+p(ku(i,j,k))
      enddo
      enddo
      enddo

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=xmin+i*dx
       yc(i,j)=ymin+j*dy
       if(ku(i,j,0).ne.0) zc(i,j)=zc(i,j)/ku(i,j,0)
      enddo
      enddo
      ncx=ncx-1
      ncy=ncy-1

c --- Write data for gnuplot
      call writegnuplot
      if(idis.eq.1) write(6,*) 'writegnuplot has been finished.'

      end
c
c     loading data: ind.profile
c     unloading data: ind1.profile
c
      subroutine ldcurve
      use variables
      use parameters
      use tool
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: 
     & d,fx,fy,fz,a1,a2,b1,b2
      character (len=80) :: com

c --- Select 0: loading only, 1: loading and unloading.
      isw=0

c --- Calc. loading curve
      if(idis.eq.1) write(6,*) 'loading curve is started.'

      nmax=10000
      allocate(d(nmax),fx(nmax),fy(nmax),fz(nmax))

c --- Read ind.profile
      open(10,file='ind.profile',status='old')
c      open(10,file='ind.d',status='old')
      read(10,100) com
      read(10,100) com
      n=1
      do i=1,nmax
       read(10,*,end=99) m,d(n),fx(n),fy(n),fz(n)
       n=n+1
      enddo
99    continue
100   format(a80)
      n=n-1
      write(6,*) 'n =',n
      close(10)

c --- Convert unit
c --- Dislpacement: Angstrom to nm
c --- Force: eV/Angstrom to mairo-N
      a2m=1.0e-10
      fac=ev2j/a2m*1.0e6
      do i=1,n
       d(i)=d(i)*a2n
       fx(i)=fx(i)*fac
       fy(i)=fy(i)*fac
       fz(i)=fz(i)*fac
      enddo

      ist=1
      do i=1,n
c       if(fz(i).gt.0.01d0) goto 1000
c       if(fz(i).gt.0.0001d0) goto 1000
       if(fz(i).gt.0.0d0) goto 1000
       ist=i
c       write(6,*) ist,fz(i)
      enddo
1000  continue

c --- Determination of contact point
      ist=1

      izero=0
      if(izero.eq.1) then

c --- Hertz: Displacement (nm), Force (micro-N)
       dher=0.1e-1
       fher=0.60667e-3

       dfh=fher/dher
       if(idis.eq.1) write(6,*) 'df/dd =',dfh
       dd=d(1)-d(2)
       do i=1,n-1
        dfi=(fz(i+1)-fz(i))/dd
c        write(6,*) i,dfi
        if(dfi.gt.dfh) then
         ist=i
         goto 1001
        endif
       enddo
1001  continue

      endif

      if(idis.eq.1) write(6,*) 'ist =',ist

      n=n-ist+1
      d0=d(ist)
      do i=1,n
       j=i+ist-1
       d(i)=d0-d(j)
       fx(i)=fx(j)
       fy(i)=fy(j)
       fz(i)=fz(j)
      enddo
      if(idis.eq.1) write(6,*) 'n =',n

c --- Write ind
      open(20,file='ind',status='unknown')
      write(20,200)
      do i=1,n
c       write(20,300) d(i),fx(i),fy(i),fz(i)
       write(20,300) d(i),fx(i),fy(i),fz(i)
      enddo
      close(20)
200   format('# Displacement (nm), Force (micro-N)')
300   format(4e15.7)

c --- Calc. hardness
      if(idis.eq.1) write(6,*) 'hardness is started.'

      allocate(a1(nmax),a2(nmax),b1(nmax),b2(nmax))
      pi=acos(-1.0d0)
      gpa=1.0d3

c --- Indenter radius
c --- ri = 10
      r=8.72d0
c      n=350
c --- ri = 15
c      r=13.08d0
c      n=375
c --- ri = 20
c      r=17.44d0

c --- A=2*pi*R*h
      pre=2.0*pi*r
      do i=1,n
       a1(i)=pre*d(i)
       if(a1(i).gt.0.0d0) b1(i)=fz(i)/a1(i)*gpa
      enddo

c --- A=pi*(2*R*h-h*h)
      do i=1,n
       a2(i)=pi*(2.0d0*r*d(i)-d(i)*d(i))
c       a2(i)=pi*(2.0d0*r*d(i)-d(i)*d(i))*1.05d0
       if(a2(i).gt.0.0d0) b2(i)=fz(i)/a2(i)*gpa
      enddo

c --- A=4*pi*r^2*(1-sin(theta))*0.5
c      m=n
c      h=d(m)
c      aa=sqrt(2.0d0*r*h-h*h)
c      ss=(r-h)/r
c      a3=4.0d0*pi*r*r*(1.0d0-ss)*0.5d0
c      b3=fz(m)/a3*gpa
c      write(6,*) r-h,r,ss
c      write(6,*) m,fz(m)
c      write(6,*) a1(m),b1(m)
c      write(6,*) a2(m),b2(m)
c      write(6,*) a3,b3

c --- Write hardness
      open(30,file='hardness',status='unknown')
      write(30,400)
      do i=1,n
       write(30,500) d(i),a1(i),b1(i),a2(i),b2(i)
      enddo
      close(30)
400   format('# Displacement Area Hardness')
500   format(5e15.7)

c --- Calc. unloading curve
      if(isw.eq.1) then
      if(idis.eq.1) write(6,*) 'unloading curve is started.'

c --- Read ind1.profile
      open(10,file='ind1.profile',status='old')
      read(10,100) com
      read(10,100) com
      n=1
      do i=1,nmax
       read(10,*,end=88) m,d(n),fx(n),fy(n),fz(n)
       n=n+1
      enddo
88    continue
      n=n-1
      close(10)

c --- Convert unit
c --- Dislpacement: Angstrom to nm
c --- Force: eV/Angstrom to mairo-N
      a2m=1.0e-10
      fac=ev2j/a2m*1.0e6
      do i=1,n
       d(i)=d(i)*a2n
       fx(i)=fx(i)*fac
       fy(i)=fy(i)*fac
       fz(i)=fz(i)*fac
      enddo

      n=n-ist+1
      do i=1,n
       d(i)=d0-d(i)
      enddo
      if(idis.eq.1) write(6,*) 'n =',n

c --- Write ind
      open(20,file='ind1',status='unknown')
      write(20,200)
      do i=1,n
c       write(20,300) d(i),fx(i),fy(i),fz(i)
       write(20,300) d(i),fx(i),fy(i),fz(i)
      enddo
      close(20)

      endif

      end
c
c     loading data: ind0.profile
c     unloading data: ind.profile
c
      subroutine unload
      use variables
      use parameters
      use tool
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: 
     & d,fx,fy,fz,d1,fx1,fy1,fz1
      character (len=80) :: com

      nmax=10000
      allocate(d(nmax),fx(nmax),fy(nmax),fz(nmax))
      allocate(d1(nmax),fx1(nmax),fy1(nmax),fz1(nmax))

c --- Read ind.profile of unloading
      open(10,file='ind.profile',status='old')
c      open(10,file='ind.d',status='old')
      read(10,100) com
      read(10,100) com
      n=1
      do i=1,nmax
       read(10,*,end=99) m,d(n),fx(n),fy(n),fz(n)
       n=n+1
      enddo
99    continue
100   format(a80)
      n=n-1
      write(6,*) 'n =',n
      close(10)

c --- Read ind.profile of loading
      open(30,file='ind0.profile',status='old')
c      open(30,file='ind0.d',status='old')
      read(30,110) com
      read(30,110) com
      n1=1
      do i=1,nmax
       read(30,*,end=98) m,d1(n1),fx1(n1),fy1(n1),fz1(n1)
       n1=n1+1
      enddo
98    continue
110   format(a80)
      n1=n1-1
      write(6,*) 'n1 =',n1
      close(30)

c --- Convert unit
c --- Dislpacement: Angstrom to nm
c --- Force: eV/Angstrom to mairo-N
      a2m=1.0e-10
      fac=ev2j/a2m*1.0e6
      do i=1,n
       d(i)=d(i)*a2n
       fx(i)=fx(i)*fac
       fy(i)=fy(i)*fac
       fz(i)=fz(i)*fac
      enddo
      do i=1,n1
       d1(i)=d1(i)*a2n
       fx1(i)=fx1(i)*fac
       fy1(i)=fy1(i)*fac
       fz1(i)=fz1(i)*fac
      enddo

      ist=1
      do i=1,n1
c       if(fz1(i).gt.0.01d0) goto 1000
c       if(fz1(i).gt.0.0d0) goto 1000
       if(fz1(i).ge.0.0d0) goto 1000
       ist=i
c       write(6,*) ist,fz(i)
      enddo
1000  continue
c      ist=1
      if(idis.eq.1) write(6,*) 'ist =',ist

c --- unloading
      n=n-ist+1
      d0=d1(ist)
      do i=1,n
       d(i)=d0-d(i)
      enddo
      if(idis.eq.1) write(6,*) 'n =',n

c --- loading
      n1=n1-ist+1
      d0=d1(ist)
      do i=1,n1
       j=i+ist-1
       d1(i)=d0-d1(j)
       fx1(i)=fx1(j)
       fy1(i)=fy1(j)
       fz1(i)=fz1(j)
      enddo
      if(idis.eq.1) write(6,*) 'n1 =',n1

c --- Write ind of unloading
      open(20,file='ind',status='unknown')
      write(20,200)
      do i=1,n
c       write(20,300) d(i),fx(i),fy(i),fz(i)
       write(20,300) d(i),fx(i),fy(i),fz(i)
      enddo
      close(20)
200   format('# Displacement Force')
300   format(4e15.7)

c --- Write ind of loading
      open(40,file='ind0',status='unknown')
      write(40,200)
      do i=1,n1
c       write(20,300) d(i),fx(i),fy(i),fz(i)
       write(40,300) d1(i),fx1(i),fy1(i),fz1(i)
      enddo
      close(40)
      end
c
      subroutine hertz
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: ht,pi

      nmx=1000
      allocate(ht(0:nmx),pi(0:nmx))

      do i=0,nmx
       ht(i)=0.0d0
       pi(i)=0.0d0
      enddo

c --- Elastic constants
      call elastic
      stop

c --- For 3C-SiC, 12-Luo-JECS
      ym=314.55d0
      pr=0.267d0
c --- For 4H-SiC, 11-Ravindra
c      ym=330.0d0
c      pr=0.212d0
      es=ym/(1.0d0-pr*pr)
      if(idis.eq.1) write(6,*) 'E^* (GPa) =',es
      ac=0.872d0
      ri=10.0d0*ac
      if(idis.eq.1) write(6,*) 'R (nm) =',ri
      fac=4.0d0/3.0d0*es*1.0d9*sqrt(ri*1.0d-9)
      if(idis.eq.1) write(6,*) 'fac =',fac

c      fac=1.5d7
      n=400
      dh=0.01d0
      hs=0.6d0
      do i=0,n
       ht(i)=i*dh
       pi(i)=fac*(ht(i)*1.0d-9)**1.5
       pi(i)=pi(i)*1.0d6
c       pi(i)=0.70d0*ht(i)**1.5
       ht(i)=ht(i)+hs
      enddo

c --- Write data
      open(10,file='hertz',status='unknown')
      write(10,100)
      do i=0,n
       write(10,200) ht(i),pi(i)
c       write(6,200) ht(i),pi(i)
      enddo
      close(10)
100   format('# Displacement (nm), load (micro-N)')
200   format(2e15.7)

      end
c
      subroutine slice
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: wx,wy,wz,iw

c --- Read dump file
      call readdump
      if(idis.eq.1) write(6,*) 'readdump has been finished.'

      xlen=xmax-xmin
      ylen=ymax-ymin
      zlen=zmax-zmin
      do i=1,n
       x(i)=x(i)*xlen
       y(i)=y(i)*ylen
       z(i)=z(i)*zlen
      enddo

      nty=1
      do i=1,n
       if(is(i).gt.nty) nty=is(i)
      enddo
      if(idis.eq.1) write(6,*) 'nty =',nty

c --- Slice
      allocate(wx(n),wy(n),wz(n),iw(n))
      xc=(xmax-xmin)*0.5d0
      x0=xc-5.0d0
      x1=xc+5.0d0
      yc=(ymax-ymin)*0.5d0
      y0=yc-5.0d0
      y1=yc+5.0d0
c      y0=ymin
c      y1=ymax
      nd=0
      do i=1,n
c       if(x(i).ge.x0.and.x(i).le.x1) then
       if(y(i).ge.y0.and.y(i).le.y1) then
        nd=nd+1
        wx(nd)=x(i)
        wy(nd)=y(i)
        wz(nd)=z(i)
        iw(nd)=is(i)
       endif
      enddo
      n=nd
      if(idis.eq.1) write(6,*) 'n =',n
      do i=1,n
       x(i)=wx(i)
       y(i)=wy(i)
       z(i)=wz(i)
       is(i)=iw(i)
      enddo

c --- Write dump file
      call writedump
      if(idis.eq.1) write(6,*) 'writedump has been finished.'

      end
c
      subroutine readdpst
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)
      character (len=80) :: com

      open(10,file='dump.stress',status='old')

      read(10,100) com
      if(idis.eq.1) write(6,100) com
      read(10,100) com
      if(idis.eq.1) write(6,100) com
      read(10,100) com
      if(idis.eq.1) write(6,100) com
      read(10,*) n
      if(idis.eq.1) write(6,*) 'n =',n
      allocate(x(n),y(n),z(n),is(n),s1(n),s2(n),s3(n),p(n))
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
       read(10,*) ii,is(i),x(i),y(i),z(i),dd,s1(i),s2(i),s3(i)
      enddo

      close(10)
100   format(a80)

      end
c
      subroutine writedpst
      use variables
      use parameters
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'writedpst is started.'

c --- Write data.file for LAMMPS
      if(idis.eq.1) write(6,*) 'Writing dump.stress1.'
      if(idis.eq.1) write(6,*) n,'atoms'
      if(idis.eq.1) write(6,*) xmin,xmax,'xlo xhi'
      if(idis.eq.1) write(6,*) ymin,ymax,'ylo yhi'
      if(idis.eq.1) write(6,*) zmin,zmax,'zlo zhi'

      open(10,file='dump.stress1',status='unknown')
      write(10,100)
100   format('ITEM: TIMESTEP')
      write(10,*) '0'
      write(10,200)
200   format('ITEM: NUMBER OF ATOMS')
      write(10,*) n
      write(10,300)
300   format('ITEM: BOX BOUNDS pp pp pp')
      write(10,*) xmin,xmax
      write(10,*) ymin,ymax
      write(10,*) zmin,zmax
      write(10,400)
400   format('ITEM: ATOMS type x y z pres')
      do i=1,n
       write(10,10010) is(i),x(i),y(i),z(i),p(i)
c       write(6,*) i,is(i),x(i),y(i),z(i)
      enddo
      close(10)
10010 format(i6,4f15.7)

      end
c
      subroutine concen
      use variables
      use parameters
      use contour
      implicit doubleprecision (a-h,o-z)
      integer,dimension(:,:,:),allocatable :: ku
      integer,dimension(:,:),allocatable :: nhy,npd

      if(idis.eq.1) write(6,*) 'concen is started.'

      ncx=30
      ncy=30
      allocate(xc(0:ncx,0:ncy),yc(0:ncx,0:ncy),zc(0:ncx,0:ncy))
      allocate(nhy(0:ncx,0:ncy),npd(0:ncx,0:ncy))

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=0.0d0
       yc(i,j)=0.0d0
       zc(i,j)=0.0d0
       nhy(i,j)=0
       npd(i,j)=0
      enddo
      enddo

c --- Read dump file
      call readdump
      if(idis.eq.1) write(6,*) 'readdump has been finished.'

c      na=n/ncx/ncy
      na=n
      allocate(ku(0:ncx,0:ncy,0:na))

      do i=0,ncx
      do j=0,ncy
      do k=0,na
       ku(i,j,k)=0
      enddo
      enddo
      enddo

      dx=(xmax-xmin)/ncx
      dy=(ymax-ymin)/ncy

      do i=1,n
       ix=(x(i)-xmin)/dx
       iy=(y(i)-ymin)/dy
       ku(ix,iy,0)=ku(ix,iy,0)+1
       ku(ix,iy,ku(ix,iy,0))=i
      enddo

      do i=0,ncx
      do j=0,ncy
      do k=1,ku(i,j,0)
       if(is(ku(i,j,k)).eq.1) npd(i,j)=npd(i,j)+1
       if(is(ku(i,j,k)).eq.2) nhy(i,j)=nhy(i,j)+1
      enddo
      enddo
      enddo

      do i=0,ncx
      do j=0,ncy
       xc(i,j)=i*dx
       yc(i,j)=j*dy
       if(npd(i,j).ne.0) zc(i,j)=real(nhy(i,j))/real(npd(i,j))
      enddo
      enddo

c      if(idis.eq.1) then
c      do i=0,ncx
c      do j=0,ncy
c       if(zc(i,j).gt.0.0d0) write(6,*) i,j,npd(i,j),nhy(i,j),zc(i,j)
c      enddo
c      enddo
c      endif

c      ncx=ncx-1
c      ncy=ncy-1

c --- Write data for gnuplot
      call writegnuplot
      if(idis.eq.1) write(6,*) 'writegnuplot has been finished.'

      end
c
      subroutine writegnuplot
      use parameters
      use contour
      implicit doubleprecision (a-h,o-z)

      if(idis.eq.1) write(6,*) 'writegnuplot is started.'

      xcmin=xc(0,0)
      xcmax=xc(0,0)
      ycmin=yc(0,0)
      ycmax=yc(0,0)
      zcmin=zc(0,0)
      zcmax=zc(0,0)

      do i=0,ncx
      do j=0,ncy
       if(xc(i,j).lt.xcmin) xcmin=xc(i,j)
       if(xc(i,j).gt.xcmax) xcmax=xc(i,j)
       if(yc(i,j).lt.ycmin) ycmin=yc(i,j)
       if(yc(i,j).gt.ycmax) ycmax=yc(i,j)
       if(zc(i,j).lt.zcmin) zcmin=zc(i,j)
       if(zc(i,j).gt.zcmax) zcmax=zc(i,j)
      enddo
      enddo

      write(6,*) 'xcmin,xcmax = ',xcmin,xcmax
      write(6,*) 'ycmin,ycmax = ',ycmin,ycmax
      write(6,*) 'zcmin,zcmax = ',zcmin,zcmax

      open(10,file='cntr',status='unknown')

      do i=0,ncx
      do j=0,ncy
       write(10,10000) xc(i,j),yc(i,j),zc(i,j)
      enddo
      write(10,*)
      enddo

10000 format(3e15.7)

      end
c
      subroutine stack
      implicit doubleprecision (a-h,o-z)
      doubleprecision,dimension(:),allocatable :: x,y,z
      character (len=80) :: com

      nn=21
      allocate(x(nn),y(nn),z(nn))

c --- Read data
      open(10,file='sfe',status='old')
      read(10,100) com
      read(10,*) xlen,ylen
      write(6,*) 'xlen,ylen:',xlen,ylen 
      read(10,100) com
      n=1
      do i=1,nn
       read(10,*,end=99) x(n),y(n)
       n=n+1
      enddo
99    continue
100   format(a80)
      n=n-1
      write(6,*) 'n =',n

c --- Check input data
      do i=1,n
       write(6,200) x(i),y(i)
      enddo

c --- 1 eV = 1.60219*10^{-19} J
      engj=1.60219d-19
c --- Area
      aa=xlen*ylen
c --- Calculation
c --- eV/A^{2} -> mJ/m^{2}
      cc=engj/aa*1.0d23
      do i=1,n
       z(i)=(y(i)-y(1))*cc
      enddo

c --- Write data
      open(20,file='stack',status='unknown')
      do i=1,n
       write(20,200) x(i),z(i)
      enddo
200   format(2e15.7)

c --- Check output data
      write(6,*) 'n =',n
      do i=1,n
       write(6,200) x(i),z(i)
      enddo

      end
