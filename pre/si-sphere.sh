atomsk --create diamond 5.431 Si -duplicate 30 30 30 si_supercell.xsf
atomsk si_supercell.xsf -select out sphere 0.5*box 0.5*box 0.5*box 70 \
-rmatom select si_sphere.lmp
