# NOTE: This script can be modified for different pair styles 
# See in.elastic for more info.

# Choose potential
#pair_style	sw
#pair_coeff * * Si.sw Si

# SiC
#pair_style      vashishta
#pair_coeff      * * SiC.vashishta Si C
pair_style      tersoff
pair_coeff      * * SiC_Erhart-Albe.tersoff Si C

# InP
#include InP_JCPA2020.snap

# Setup neighbor style
neighbor 1.0 nsq
neigh_modify once no every 1 delay 0 check yes

# Setup minimization style
min_style	     cg
min_modify	     dmax ${dmax} line quadratic

# Setup output
thermo		1
thermo_style custom step temp pe press pxx pyy pzz pxy pxz pyz lx ly lz vol
thermo_modify norm no
