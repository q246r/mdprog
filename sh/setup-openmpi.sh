#!/bin/sh

# Preparation:
#  You have to check up on the latest version. 
# Usage:
#  # cd /usr/local/src
#  # cp /home/q246r/mdprog/sh/setup-openmpi.sh .
#  # ./setup-openmpi.sh

version="openmpi-4.0.2"
filename=$version".tar.gz"

wget --no-check-certificate https://www.open-mpi.org/software/ompi/v4.0/downloads/$filename
tar -zxvf $filename
cd $version
./configure --prefix=/usr/local/openmpi-4.0.2 CC=gcc CXX=g++ F77=gfortran FC=gfortran
make all
make install
