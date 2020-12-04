#!/bin/bash
grep ! ecut_*.out | awk '{print $1, $5}' | sed -e 's/ecut_//g' -e 's/.out:!//g' > ecut.txt
