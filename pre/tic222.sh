#!/bin/bash

atomsk --create rocksalt 1.0 Ti C unitcell.xsf
atomsk unitcell.xsf -duplicate 2 2 2 tic222.xsf
cat tic222.xsf | sed 1,12d > dump1
cat dump1 | sed -e "s/22/Ti/g" -e "s/6/C /g" > tic222.coord
rm unitcell.xsf dump*
