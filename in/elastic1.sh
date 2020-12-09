#!/bin/bash
target1=4.335321212
echo $target1
for i in -0.01 -0.005 0.005 0.01;
do
  target2=`echo "(1.0 + $i) * $target1" | bc`
  echo $i $target2
  title=elastic_$i
  cat elastic1.in | sed -e "s/target1/$target1/g" -e "s/target2/$target2/g" > $title.in
  mpirun -np 28 pw.x_openmpi < $title.in > $title.out
done
