#!/bin/sh

# Compile Quantum ESPRESSO
# Execute your home directory
# Set path myqe/bin

#sudo apt install -y libblas-dev liblapack-dev
git clone https://github.com/QEF/q-e.git myqe

cd myqe
./configure
make all
