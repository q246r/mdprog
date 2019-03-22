#!/bin/bash

fname="stress.d"
dt=0.0001
b2p=0.0001

grep -v "#" $fname | awk '{print $1*0.0001,$2*0.0001}'
