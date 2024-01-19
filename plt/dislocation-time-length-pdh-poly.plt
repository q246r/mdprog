# ファイル出力
#set terminal postscript eps color enhanced "Arial, 18" size 8cm, 6cm
#set output "fig.eps"
# emfファイル出力(ワードに挿入するため)
set terminal emf color enhanced font "Arial,18" size 640,480 linewidth 1.5
set output "fig.emf"

# 軸の設定
set xlabel "{/=20 Time (ns)}" offset 0.0,0.4
set ylabel "{/=20 Dislocation length (nm)}" offset 0.0,0.0
set xrange [-0.05:1.05]
set format x "%2.1f"
set yrange [0:4700]
set xtics 0.2
set ytics 1000
set border lw 1.5

# グラフの設定
set key left at graph 0.03,0.95 Left reverse samplen 2
#unset key

plot \
"240116-240115a-Dislocation-length-time.txt" u 1:2 lt 1 lw 1.5 pt 7 ps 1.0 lc rgb "red" title "grain 4" with linespoints, \
"240116-240115b-Dislocation-length-time.txt" u 1:2 lt 1 lw 1.5 pt 5 ps 1.3 lc rgb "blue" title "grain 12" with linespoints, \
