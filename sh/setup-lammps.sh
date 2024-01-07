#!/bin/bash

cd ~
git clone -b stable https://github.com/lammps/lammps.git mylammps

cd ~/mylammps/src
make yes-manybody
make yes-ml-snap
make -j 8 mpi

# cp lmp_mpi ~/bin/lmp_mpi_new
