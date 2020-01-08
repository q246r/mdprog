#!/bin/sh

# Preparation:
#  You have to check up on the latest version. 
# Usage:
#  $ cd ~
#  $ ./setup-openmpi-user.sh

version="openmpi-4.0.2"

cp .bashrc bashrc.backup
echo "MPIROOT=/usr/local/$version" >> .bashrc
echo 'export PATH=$MPIROOT/bin:$PATH' >> .bashrc
echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MPIROOT/lib' >> .bashrc
echo 'export MANPATH=$MANPATH:$MPIROOT/share/man' >> .bashrc
