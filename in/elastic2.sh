#!/bin/bash
target1=4.335321212
echo $target1
for i in -0.06 -0.03 0.03 0.06;
do
  target2=`echo "$i * $target1" | bc`
  echo $i $target2
  title=elastic_$i
  cat elastic2.in | sed -e "s/target1/$target1/g" -e "s/target2/$target2/g" > $title.in
  mpirun -np 28 pw.x_openmpi < $title.in > $title.out
done
