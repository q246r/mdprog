#!/bin/bash
#PBS -N test
#PBS -j oe
#PBS -l nodes=1:ppn=16
#PBS -q default

cd $PBS_O_WORKDIR

mpirun -np 16 ~/bin/lmp < in.eam
