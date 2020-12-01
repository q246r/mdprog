#!/bin/bash

grep 'total energy' scf.out | sed 's/!//g' | awk '{print NR, $4}' > temp
sed '$d' temp > temp2
rm temp
mv temp2 temp
