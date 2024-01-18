atomsk --create fcc 3.885 Pd pd_unit.xsf
# create grainID
#atomsk --polycrystal pd_unit.xsf poly.dat pd.cfg -wrap
# for lammps
atomsk --polycrystal pd_unit.xsf poly.dat pd.lmp -wrap
