# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "ortho.tex"

set key bottom right vertical spacing 1.75 box width 4
set logscale x 2
set logscale y 10

set xrange [1:8192]
set xlabel "$l$ (matrix order $n=2^l$)"
set xtics ("1" 2, "2" 4, "3" 8, "4" 16, "5" 32, "6" 64, "7" 128, "8" 256, "9" 512, "10" 1024, "11" 2048, "12" 4096)
set ylabel "departure from ($J$-)unitarity"
set yrange [1e-17:1e-9]
set ytics ("$10^{-17}$" 1e-17, "$10^{-16}$" 1e-16, "$10^{-15}$" 1e-15, "$10^{-14}$" 1e-14, "$10^{-13}$" 1e-13, "$10^{-12}$" 1e-12, "$10^{-11}$" 1e-11, "$10^{-10}$" 1e-10, "$10^{-9\\hphantom{0}}$" 1e-9)

set datafile columnheaders
set multiplot
plot "d-0.csv" using 1:11 with points title "$\\rho_V^{\\mathbb{R}}[\\mathtt{dR}]$" pointtype 15 linetype 1, \
     "d-0.csv" using 1:10 with points title "$\\rho_U^{\\mathbb{R}}[\\mathtt{dR}]$" pointtype 13 linetype 7
set key top left vertical spacing 1.75 box
plot "z-2.csv" using 1:11 with points title "$\\rho_V^{\\mathbb{C}}[\\mathtt{rc}]$" pointtype 5 linetype -1, \
     "z-0.csv" using 1:11 with points title "$\\rho_V^{\\mathbb{C}}[\\mathtt{dR}]$" pointtype 7 linetype 2, \
     "z-2.csv" using 1:10 with points title "$\\rho_U^{\\mathbb{C}}[\\mathtt{rc}]$" pointtype 11 linetype 4, \
     "z-0.csv" using 1:10 with points title "$\\rho_U^{\\mathbb{C}}[\\mathtt{dR}]$" pointtype 9 linetype 6
unset multiplot
