#!/bin/sh

# Compile LAMMPS
# $ mkdir ~/bin
# Execute your home directory

git clone -b stable https://github.com/lammps/lammps.git mylammps

cd mylammps/src
make yes-kspace
make yes-manybody
make yes-molecule
make mpi

cp lmp_mpi ~/bin/lmp_mpi_new
