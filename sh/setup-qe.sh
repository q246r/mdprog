#!/bin/sh

sudo apt install -y libblas-dev liblapack-dev
git clone https://github.com/QEF/q-e.git myqe

cd myqe

# GNU compiler
./configure F90=gfortran F77=gfortran CC=gcc MPIF90=mpif90 --enable-openmp --enable-parallel
# Intel Parallel Studio XE
#./configure MPIF90=mpiifort F90=ifort F77=ifort CC=gcc --enable-openmp --enable-parallel --with-scalapack=inte
# Intel OneAPI
#./configure MPIF90=mpiifort F90=ifort F77=ifort CC=icc --enable-openmp --enable-parallel --with-scalapack=intel

make -j 4 all
