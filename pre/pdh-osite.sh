# PdHx: H located at O-site
atomsk --create fcc 3.885 Pd -duplicate 1 1 1 pdh.atsk
atomsk pdh.atsk -add-atom H at 0.5*box 0.0*box 0.0*box pdh.lmp
cp pdh.lmp data.file
rm pdh.atsk pdh.lmp
