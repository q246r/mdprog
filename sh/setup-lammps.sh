#!/bin/sh

git clone -b stable https://github.com/lammps/lammps.git mylammps

cd mylammps/src
make yes-manybody
make -j 4 mpi

# cp lmp_mpi ~/bin/lmp_mpi_new
