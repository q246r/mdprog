# PdHx: H located at T-site
atomsk --create fcc 3.885 Pd -duplicate 2 2 2 pdh.atsk
atomsk pdh.atsk -add-atom H at 0.97125 0.97125 0.97125 pdh.lmp
cp pdh.lmp data.file
rm pdh.atsk pdh.lmp
