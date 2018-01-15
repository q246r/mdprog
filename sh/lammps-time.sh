#!/bin/sh

# Extract elapsed time from LAMMPS log file

cp ../171219a/log.lammps log.lammps1
cp ../171219b/log.lammps log.lammps2
cp ../171219c/log.lammps log.lammps3
cp ../171219d/log.lammps log.lammps4

grep Loop log.lammps* | awk '{print $4}' > result
rm log.lammps*
