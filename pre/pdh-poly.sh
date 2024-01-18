atomsk --create rocksalt 3.885 Pd H -duplicate 5 5 5 pdh.atsk
atomsk pdh.atsk -select random 50.0% H -rmatom select pdh_unit.xsf
rm pdh.atsk
# create grainID
#atomsk --polycrystal pdh_unit.xsf poly.dat pdh.cfg -wrap
# for lammps
atomsk --polycrystal pdh_unit.xsf poly.dat pdh.lmp -wrap
