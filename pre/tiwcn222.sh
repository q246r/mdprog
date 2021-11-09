#!/bin/bash

atomsk --create rocksalt 1.0 Ti C unitcell.xsf
atomsk unitcell.xsf -duplicate 2 2 2 dump1.xsf
atomsk dump1.xsf -select random 16 C -substitute C N dump2.xsf
atomsk dump2.xsf -select random 1 Ti -substitute Ti W tiwcn222.xsf
cat tiwcn222.xsf | sed 1,12d > dump3
cat dump3 | sed -e "s/22/Ti/g" -e "s/74/W /g" -e "s/6/C /g" -e "s/7/N /g" > tiwcn222.coord
rm unitcell.xsf dump*
