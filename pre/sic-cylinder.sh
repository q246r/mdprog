atomsk --create zincblende 4.3581 Si C orient [100] [010] [001] -duplicate 10 10 10 sic_supercell.xsf
atomsk sic_supercell.xsf -select out cylinder X 0.5*box 0.5*box 20 \
-rmatom select sic_cylinder.lmp
