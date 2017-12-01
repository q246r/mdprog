# Pd edge dislocation dipole
atomsk --create fcc 3.885 Pd orient [1-10] [111] [-1-12] \
-duplicate 40 40 2 pd.atsk
atomsk pd.atsk \
-dislocation 0.251*box 0.251*box edge z y -2.747 0.415 \
-dislocation 0.751*box 0.751*box edge z y 2.747 0.415 \
pd.lmp
cp pd.lmp data.file
rm pd.atsk pd.lmp
