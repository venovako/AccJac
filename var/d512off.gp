# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "d512off.tex"
set key bottom left vertical spacing 2 box

set logscale y

set xrange [-43000:1620000]
set xtics ("1" 1, "2" 130817, "3" 261633, "4" 392449, "5" 523265, "6" 654081, "7" 784897, "8" 915713, "9" 1046529, "10" 1177345, "11" 1308161, "12" 1438977, "$\\rightarrow$" 1569793)
set xlabel "sweep in double precision"

set yrange [1e-14:4e3]
set ylabel "$\\mathop{\\mathrm{off}}(A)$"
set ytics ("$10^{-13}$" 1e-13, "$10^{-11}$" 1e-11, "$10^{-9\\hphantom{0}}$" 1e-9, "$10^{-7\\hphantom{0}}$" 1e-7, "$10^{-5\\hphantom{0}}$" 1e-5, "$10^{-3\\hphantom{0}}$" 1e-3, "$10^{-1\\hphantom{0}}$" 1e-1, "$10^{1\\hphantom{-0}}$" 1e1, "$10^{3\\hphantom{-0}}$" 1e3)

set datafile columnheaders
plot "d512rc.csv" using 1:3 with points title "row-cyclic" pointtype 7 linetype 1, \
     "d512dR.csv" using 1:3 with points title "de\\,Rijk" pointtype 7 linetype -1
