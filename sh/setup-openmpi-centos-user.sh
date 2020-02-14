#!/bin/sh

# openmpi package install
#  # yum install -y openmpi3 openmpi3-devel
#  $ cd ~
#  $ ./setup-openmpi-centos-user.sh
#  $ source .bashrc

cp .bashrc bashrc.backup
echo 'MPIROOT=/usr/lib64/openmpi3' >> .bashrc
echo 'export PATH=$MPIROOT/bin:$PATH' >> .bashrc
echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MPIROOT/lib' >> .bashrc
