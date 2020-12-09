#!/bin/bash
for i in -0.01 -0.005 0.005 0.01;
do
  echo $i >> temp1 
  grep kbar -A 3 elastic_$i.out | sed -n '2,2p' | awk '{print $4}' >> temp2
  grep kbar -A 3 elastic_$i.out | sed -n '3,3p' | awk '{print $5}' >> temp3
done
paste temp1 temp2 > s11e.txt
paste temp1 temp3 > s22e.txt
rm temp*
