#!/bin/sh

# Parallel efficiency of lammps benchmarks

datadir=180118

# Strong scaling
cp ../$datadir'a'/result .
./para-eff-s
cp efficiency efficiency-s20
cp ../$datadir'b'/result .
./para-eff-s
cp efficiency efficiency-s40
cp ../$datadir'c'/result .
./para-eff-s
cp efficiency efficiency-s80

# Weak scaling
cp ../$datadir'd'/result .
./para-eff-w
cp efficiency efficiency-w20
cp ../$datadir'e'/result .
./para-eff-w
cp efficiency efficiency-w40
cp ../$datadir'f'/result .
./para-eff-w
cp efficiency efficiency-w80

rm result efficiency
