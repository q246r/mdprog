#!/bin/sh

# Diffusivity is calculated from mean square displacement (msd).
# D, Dx, Dy, and Dz are output.

datadir='171219'

file1=$datadir'a'/msd.d
file2=$datadir'b'/msd.d
file3=$datadir'c'/msd.d
file4=$datadir'd'/msd.d
echo $file1
echo $file2
echo $file3
echo $file4

cp ../$file1 .
./lpost-msd > data1
cp ../$file2 .
./lpost-msd > data2
cp ../$file3 .
./lpost-msd > data3
cp ../$file4 .
./lpost-msd > data4

grep 'D :' data* | awk '{print $4*1.0e10}' > Dd
grep 'Dx:' data* | awk '{print $3*1.0e10}' > Dx
grep 'Dy:' data* | awk '{print $3*1.0e10}' > Dy
grep 'Dz:' data* | awk '{print $3*1.0e10}' > Dz

paste -d " " Dd Dx Dy Dz > result
rm data* D?
