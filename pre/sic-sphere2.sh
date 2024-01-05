atomsk --create zincblende 4.3706 Si C orient [100] [010] [001] -duplicate 32 32 25 sic_supercell.xsf
atomsk sic_supercell.xsf -select out sphere 0.5*box 0.5*box 0.5*box 30 \
-rmatom select -properties sic.txt sic_sphere.lmp
