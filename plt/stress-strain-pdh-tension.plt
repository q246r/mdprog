# ファイル出力
#set terminal postscript eps color enhanced "Arial, 18" size 8cm, 6cm
#set output "fig.eps"
set terminal emf color enhanced font "Arial,18" size 640,480
set output "fig.emf"

# 軸の設定
set xlabel "{/=20 Tensile strain}" offset 0.0,0.4
set ylabel "{/=20 Tensile stress (GPa)}" offset 0.0,0.0
set xrange [0:0.2]
set format x "%3.2f"
set yrange [0:8]
set xtics 0.05
set ytics 2
set border lw 1.5

# グラフの設定
set key left at graph 0.65,0.95 Left reverse samplen 2
#unset key

plot \
"230708a-stress.txt" u ($1)/100000000:(-$2)/10000 with lines lt 1 lw 1.5 lc rgb "red" title "{/Times:Italic x}=0", \
"230708b-stress.txt" u ($1)/100000000:(-$2)/10000 with lines lt 1 lw 1.5 lc rgb "blue" title "{/Times:Italic x}=0.05", \
"230708c-stress.txt" u ($1)/100000000:(-$2)/10000 with lines lt 1 lw 1.5 lc rgb "green" title "{/Times:Italic x}=0.5", \
