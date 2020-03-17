#!/bin/sh

# After InfiniBand drivers are installed, 
# you have to check openmpi directory.
#  $ ./setup-openmpi-ib-user.sh
#  $ source .bashrc

cp .bashrc bashrc.backup
echo 'MPIROOT=/usr/mpi/gcc/openmpi-4.0.3rc4' >> .bashrc
echo 'export PATH=$MPIROOT/bin:$PATH' >> .bashrc
echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MPIROOT/lib' >> .bashrc
echo 'export MANPATH=$MANPATH:$MPIROOT/share/man' >> .bashrc
