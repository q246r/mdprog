# ファイル出力
#set terminal postscript eps color enhanced "Arial, 18" size 8cm, 6cm
#set output "fig.eps"
set terminal emf color enhanced font "Arial,18" size 640,480
set output "fig.emf"

# 軸の設定
set xlabel "Compressive strain" offset 0.0,0.0
set ylabel "Compressive stress (GPa)" offset 0.0,0.0
set xrange [-0.02:0.5]
set yrange [-0.5:4]
set xtics 0.1
set ytics 1
set border lw 1.5

# グラフの設定
set key left at graph 0.03,0.95 Left reverse samplen 2
#unset key

plot \
"230318c-stress.txt" u ((584-$8)/584):($4/10000) with lines lt 1 lw 1.5 lc rgb "red" title "[110]", \
"230319c-stress.txt" u ((584-$8)/584):($4/10000) with lines lt 1 lw 1.5 lc rgb "blue" title "[100]", \
"240107c-stress.txt" u ((584-$8)/584):($4/10000) with lines lt 1 lw 1.5 lc rgb "green" title "[111]", \
