#!/bin/sh

# Benchmark test for LAMMPS

rm log.lammps*

mpirun -np 1 lmp < in.eam
mv log.lammps log.lammps01
mpirun -np 2 lmp < in.eam
mv log.lammps log.lammps02
mpirun -np 4 lmp < in.eam
mv log.lammps log.lammps04
mpirun -np 8 lmp < in.eam
mv log.lammps log.lammps08
mpirun -np 16 lmp < in.eam
mv log.lammps log.lammps16
mpirun -np 28 lmp < in.eam
mv log.lammps log.lammps28

grep Loop log.lammps* | awk '{print $6, $4}' > result
