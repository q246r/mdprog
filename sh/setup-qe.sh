#!/bin/sh

# Compile Quantum ESPRESSO
# $ mkdir ~/bin
# Execute your home directory

git clone https://github.com/QEF/q-e.git myqe

cd myqe
./configure
make all

#cp lmp_mpi ~/bin/lmp_mpi_new
