#!/bin/sh

# Diffusivity is calculated from mean square displacement (msd).
# D, Dx, Dy, and Dz are output.

datadir='171231'

file1=$datadir'd'/msd.d
file2=$datadir'c'/msd.d
file3=$datadir'b'/msd.d
file4=$datadir'a'/msd.d
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

grep 'D :' data* | awk '{print $4}' > Dd
grep 'Dx:' data* | awk '{print $3}' > Dx
grep 'Dy:' data* | awk '{print $3}' > Dy
grep 'Dz:' data* | awk '{print $3}' > Dz

paste -d " " Dd Dx Dy Dz > result
rm data* D?
