# NOTE: This script can be modified for different atomic structures, 
# units, etc. See in.elastic for more info.
#

# Define the finite deformation size. Try several values of this
# variable to verify that results do not depend on it.
variable up equal 1.0e-6
 
# Define the amount of random jiggle for atoms
# This prevents atoms from staying on saddle points
variable atomjiggle equal 1.0e-5

# Uncomment one of these blocks, depending on what units
# you are using in LAMMPS and for output

# metal units, elastic constants in eV/A^3
#units		metal
#variable cfac equal 6.2414e-7
#variable cunits string eV/A^3

# metal units, elastic constants in GPa
units		metal
variable cfac equal 1.0e-4
variable cunits string GPa

# real units, elastic constants in GPa
#units		real
#variable cfac equal 1.01325e-4
#variable cunits string GPa

# Define minimization parameters
variable etol equal 0.0 
variable ftol equal 1.0e-10
variable maxiter equal 100
variable maxeval equal 1000
variable dmax equal 1.0e-2

# generate the box and atom positions using a diamond lattice
#variable a equal 5.43

#boundary	p p p

#lattice         diamond $a
#region		box prism 0 2.0 0 3.0 0 4.0 0.0 0.0 0.0
#create_box	1 box
#create_atoms	1 box

# Need to set mass to something, just to satisfy LAMMPS
#mass 1 1.0e-20

# SiC
variable nrep equal 4
# vashishta
#variable a equal 4.358
# erhart
variable a equal 4.359

variable nx equal ${nrep}
variable ny equal ${nrep}
variable nz equal ${nrep}

boundary        p p p

lattice         diamond $a
region          box block 0 ${nx} 0 ${ny} 0 ${nz}
create_box      2 box
create_atoms    1 box basis 5 2 basis 6 2 basis 7 2 basis 8 2
group		all type 1 2
change_box	all triclinic

mass 1 28.0855
mass 2 12.011

# InP
#variable nrep equal 4
#variable a equal 5.83

#variable nx equal ${nrep}
#variable ny equal ${nrep}
#variable nz equal ${nrep}

#boundary        p p p

#lattice         diamond $a
#region          box block 0 ${nx} 0 ${ny} 0 ${nz}
#create_box      2 box
#create_atoms    1 box basis 5 2 basis 6 2 basis 7 2 basis 8 2
#group		all type 1 2
#change_box	all triclinic

#mass 1 114.76
#mass 2 30.98

