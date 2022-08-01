atomsk --create zincblende 4.3581 Si C -duplicate 30 30 30 sic_supercell.xsf
atomsk sic_supercell.xsf -select out sphere 0.5*box 0.5*box 0.5*box 60 \
-rmatom select sic_sphere.lmp
