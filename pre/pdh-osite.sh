# PdHx: H located at O-site
atomsk --create fcc 3.885 Pd -duplicate 2 2 2 pdh.atsk
atomsk pdh.atsk -add-atom H at 1.9425 0.0 0.0 pdh.lmp
cp pdh.lmp data.file
rm pdh.atsk pdh.lmp
