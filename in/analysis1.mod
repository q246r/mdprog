# Choose analysis

#variable	Nd equal 100
variable	Nd equal ${Ntot}/100

# Thermo-dynamics
variable	temp1 equal temp
variable	press1 equal press
variable	vol1 equal vol
variable	pe1 equal pe
# Time average
fix		myth all ave/time 1 ${Nd} ${Nd} v_temp1 v_press1 v_vol1 v_pe1 file thermo.d
# No time average
#fix		myth all ave/time 1 1 ${Nd} v_temp1 v_press1 v_vol1 v_pe1 file thermo.d

# Stress tensor
compute		st all pressure thermo_temp
# Time average
fix		myst all ave/time 1 ${Nd} ${Nd} c_st[1] c_st[2] c_st[3] c_st[4] c_st[5] c_st[6] file stress.d
# No time average
#fix		myst all ave/time 1 1 ${Nd} c_st[1] c_st[2] c_st[3] c_st[4] c_st[5] c_st[6] file stress.d

# MSD of hydrogen, atom type 2
# use in in.relax-h
#compute		hyd H msd
# Time average
# fix		mymsd all ave/time 1 ${Nd} ${Nd} c_hyd[4] c_hyd[1] c_hyd[2] c_hyd[3] file msd.d
# No time average
#fix		mymsd all ave/time 1 1 ${Nd} c_hyd[4] c_hyd[1] c_hyd[2] c_hyd[3] file msd.d
# MSD of palladium, atom type 1
#compute		pal Pd msd
#fix		msd1 all ave/time 1 ${Nd} ${Nd} c_pal[4] c_pal[1] c_pal[2] c_pal[3] file msd_pal.d
# MSD of silver, atom type 3
#compute		sil Ag msd
#fix		msd2 all ave/time 1 ${Nd} ${Nd} c_sil[4] c_sil[1] c_sil[2] c_sil[3] file msd_sil.d
