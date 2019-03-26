#!/bin/bash

fname="stress.d"
fname1="stress1.d"

echo "# t(ps), s11(GPa), s22, s33, s12, s13, s23" > $fname1
grep -v "#" $fname | awk '{print $1*0.0001,$2*0.0001,$3*0.0001,$4*0.0001,$5*0.0001,$6*0.0001,$7*0.0001}' >> $fname1
