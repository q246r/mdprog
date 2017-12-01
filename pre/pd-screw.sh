# Pd screw dislocation quadrupole
atomsk --create fcc 3.885 Pd orient [-1-12] [111] [1-10] \
-duplicate 40 30 2 pd.atsk
atomsk pd.atsk \
-dislocation 0.251*box 0.251*box screw z y 2.747 \
-dislocation 0.751*box 0.251*box screw z y -2.747 \
-dislocation 0.251*box 0.751*box screw z y -2.747 \
-dislocation 0.751*box 0.751*box screw z y 2.747 \
pd.lmp
cp pd.lmp data.file
rm pd.atsk pd.lmp
