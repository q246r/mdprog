c
c Elastic constants using VRH average
c
      program main
      implicit doubleprecision (a-h,o-z)

c idis=0: Only Hill
c idis=1: Voigt, Reuss, and Hill
      idis=0

c Input data
c      c11=518.3d0
c      c12=115.6d0
c      c44=183.0d0
c W
c      c11=510.0d0
c      c12=201.0d0
c      c44=143.0d0
c Mo
      c11=472.0d0
      c12=158.0d0
      c44=106.0d0

c Write data
      write(6,10000) 'C11 (GPa) =',c11
      write(6,10000) 'C12 (GPa) =',c12
      write(6,10000) 'C44 (GPa) =',c44

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

c Anisotropy constant
      ac=2.0d0*c44/(c11-c12)

c Output results
      if(idis.eq.1) then
      write(6,20000) 'Voigt average:'
      write(6,10000) 'Lames constants 1 =',av 
      write(6,10000) 'Lames constants 2 =',bv 
      write(6,10000) 'Youngs modulus =',ymv
      write(6,10000) 'Shear modulus =',bv 
      write(6,10000) 'Poissons ratio =',prv 
      write(6,10000) 'Bulk modulus =',bmv 
      write(6,20000) 'Reuss average:'
      write(6,10000) 'Lames constants 1 =',ar 
      write(6,10000) 'Lames constants 2 =',br 
      write(6,10000) 'Youngs modulus =',ymr
      write(6,10000) 'Shear modulus =',br 
      write(6,10000) 'Poissons ratio =',prr 
      write(6,10000) 'Bulk modulus =',bmr 
      endif
      write(6,20000) 'Hill average:'
      write(6,10000) 'Lames constants 1 =',ah 
      write(6,10000) 'Lames constants 2 =',bh 
      write(6,10000) 'Youngs modulus =',ymh
      write(6,10000) 'Shear modulus =',bh 
      write(6,10000) 'Poissons ratio =',prh 
      write(6,10000) 'Bulk modulus =',bmh 
      write(6,10000) 'B/G =',bmh/bh 
      write(6,10000) 'Aniotropy constant =',ac
      write(6,*)
10000 format(a,e15.7)
20000 format(a)

      end
