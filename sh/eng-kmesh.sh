#!/bin/bash
grep ! kmesh_*.out | awk '{print $1, $5}' | sed -e 's/kmesh_//g' -e 's/.out:!//g' > kmesh.txt
