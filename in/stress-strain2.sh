#!/bin/bash
for i in -0.06 -0.03 0.03 0.06;
do
  echo $i >> temp1 
  grep kbar -A 3 elastic_$i.out | sed -n '2,2p' | awk '{print $5}' >> temp2
done
paste temp1 temp2 > s12e.txt
rm temp*
