#!/bin/sh

# Install development environment in Ubuntu
# $ sudo -s

apt install -y gcc gfortran g++ make
apt install -y openmpi-bin libopenmpi-dev
apt install -y libblas-dev liblapack-dev
