# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "d128off.tex"
set key bottom left vertical spacing 2 box

set logscale y

set xrange [-1700:82500]
set xtics ("1" 1, "2" 8129, "3" 16257, "4" 24385, "5" 32513, "6" 40641, "7" 48769, "8" 56897, "9" 65025, "10" 73153, "$\\rightarrow$" 81281)
set xlabel "sweep in double precision"

set yrange [1e-15:1.75e2]
set ylabel "$\\mathop{\\mathrm{off}}(A)$"
set ytics ("$10^{-15}$" 1e-15, "$10^{-13}$" 1e-13, "$10^{-11}$" 1e-11, "$10^{-9\\hphantom{0}}$" 1e-9, "$10^{-7\\hphantom{0}}$" 1e-7, "$10^{-5\\hphantom{0}}$" 1e-5, "$10^{-3\\hphantom{0}}$" 1e-3, "$10^{-1\\hphantom{0}}$" 1e-1, "$10^{1\\hphantom{-0}}$" 1e1)

set datafile columnheaders
plot "128rc.csv" using 1:3 with points title "row-cyclic" pointtype 7 linetype 1, \
     "128dR.csv" using 1:3 with points title "de\\,Rijk" pointtype 7 linetype -1
