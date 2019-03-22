# PdHx
atomsk --create rocksalt 3.885 Pd H -duplicate 10 10 10 pdh.atsk
atomsk pdh.atsk -select random 50.0% H -rmatom select pdh.lmp
rm pdh.atsk
