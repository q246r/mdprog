#!/bin/bash

atomsk --create rocksalt 1.0 Ti C unitcell.xsf
atomsk unitcell.xsf -duplicate 2 2 2 dump1.xsf
atomsk dump1.xsf -select random 16 C -substitute C N ticn222.xsf
cat ticn222.xsf | sed 1,12d > dump2
cat dump2 | sed -e "s/22/Ti/g" -e "s/6/C /g" -e "s/7/N /g" > ticn222.coord
rm unitcell.xsf dump*
