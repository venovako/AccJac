# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "dx.tex"

set key bottom left horizontal spacing 1 box width 3 height 1
set logscale y 10

set xrange [-3:260]
set xlabel "$j$"
set xtics ("1" 1, "50" 50, "100" 100, "150" 150, "200" 200, "250" 250)
set yrange [1e-20:1.0001e-14]
set ylabel "relative errors in $\\sigma_j$"
set ytics ("$10^{-20}$" 1e-20, "$10^{-19}$" 1e-19, "$10^{-18}$" 1e-18, "$10^{-17}$" 1e-17, "$10^{-16}$" 1e-16, "$10^{-15}$" 1e-15, "$10^{-14}$" 1e-14)

set datafile columnheaders
plot "dx-0.csv" using 1:2 with points title "$\\rho_{\\Sigma}^{\\mathbb{R}}[\\mathtt{dR}]$" pointtype 5 linetype -1, \
     "dx-2.csv" using 1:2 with points title "$\\rho_{\\Sigma}^{\\mathbb{R}}[\\mathtt{rc}]$" pointtype 7 linetype 7
