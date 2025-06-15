# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "z512_off.tex"
set key bottom left vertical spacing 1.5 box width 2 title "\\textsc{unsorted}" offset 0,1
set logscale y

set xrange [-47000:1753609]
set xtics ("1" 1, "2" 130817, "3" 261633, "4" 392449, "5" 523265, "6" 654081, "7" 784897, "8" 915713, "9" 1046529, "10" 1177345, "11" 1308161, "12" 1438977, "13" 1569793, "$\\rightarrow$" 1700609)
set xlabel "cycle (complex double precision)"

set yrange [1e-14:4e3]
set ylabel "$\\mathop{\\mathrm{off}}(A)$"
set ytics ("$10^{-13}$" 1e-13, "$10^{-11}$" 1e-11, "$10^{-9\\hphantom{0}}$" 1e-9, "$10^{-7\\hphantom{0}}$" 1e-7, "$10^{-5\\hphantom{0}}$" 1e-5, "$10^{-3\\hphantom{0}}$" 1e-3, "$10^{-1\\hphantom{0}}$" 1e-1, "$10^{1\\hphantom{-0}}$" 1e1, "$10^{3\\hphantom{-0}}$" 1e3)

set datafile columnheaders
plot "z512_9.csv" using 1:3 with points title "Mantharam--Eberlein" pointtype 7 linetype 3 pointsize 0.6875, \
     "z512_1.csv" using 1:3 with points title "row-cyclic" pointtype 7 linetype 7 pointsize 0.6875, \
     "z512_7.csv" using 1:3 with points title "modified de\\,Rijk" pointtype 7 linetype -1 pointsize 0.6875
