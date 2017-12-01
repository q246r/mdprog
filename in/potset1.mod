# Potential

# Ni
#pair_style	eam/alloy
#pair_coeff	* * ../potentials/NiAlH_jea.eam.alloy Ni

# Ni Indentation
#pair_style	eam/alloy
#pair_coeff	* * ../potentials/NiAlH_jea.eam.alloy Ni Ni

# Ni-H
#pair_style	eam/alloy
#pair_coeff	* * ../potentials/NiAlH_jea.eam.alloy Ni H
 
# Pd-H
pair_style	eam/alloy
#pair_coeff	* * ../potentials/PdH.eam Pd H
pair_coeff	* * ../potentials/PdH_Zhou_June29_2007_2.set Pd
#pair_coeff	* * ../potentials/PdH_Zhou_June29_2007_2.set Pd H

# Pd-Ag-H
#pair_style	eam/alloy
#pair_coeff	* * ../potentials/PdAgH_HybridPd3Ag.eam.alloy Pd Ag
#pair_coeff	* * ../potentials/PdAgH_MorsePd3Ag.eam.alloy Pd Ag
#pair_coeff	* * ../potentials/PdAgH_HybridPd3Ag.eam.alloy Pd H Ag
#pair_coeff	* * ../potentials/PdAgH_MorsePd3Ag.eam.alloy Pd H Ag
#pair_coeff	* * ../potentials/PdAgH_MorsePd3Ag.eam.alloy Pd H
#pair_coeff	* * ../potentials/PdAgH_MorsePd3Ag.eam.alloy Pd

# SiC
#pair_style	tersoff
#pair_coeff	* * ../potentials/SiC_Erhart-Albe.tersoff Si C
#mass		1	28.06
#mass		2	12.01

# Group

#group		H type 2
#group		Pd type 1
#group		Ag type 3

# Time integral parameters

#timestep	1.0e-4
timestep	1.0e-3

neighbor	1.0 bin
neigh_modify	every 1 delay 5 check yes
