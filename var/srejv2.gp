# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 14cm,6cm
set output "srejv2.tex"
set key center rmargin vertical spacing 2 box

set xrange [0:32]
set xtics ("1" 1, "16" 16, "31" 31)
set xlabel "test run (each with $2^{30}$ single precision matrices)"

set ylabel "maximal relative errors / $\\varepsilon$"

set datafile columnheaders

plot "sjv2t.csv" using 1:4 with points title "$\\mathop{\\mathrm{\\rho_{\\mathbb{R}}}}(\\det V)$" pointtype 13 linetype 14, \
     "sjv2t.csv" using 1:5 with points title "$\\mathop{\\mathrm{\\rho_{\\mathbb{R}}}}(\\cosh\\phi)$" pointtype 15 linetype 1, \
     "sjv2t.csv" using 1:6 with points title "$\\mathop{\\mathrm{\\rho_{\\mathbb{R}}}}(\\sinh\\phi)$" pointtype 7 linetype 3, \
     "cjv2t.csv" using 1:4 with points title "$\\mathop{\\mathrm{\\rho_{\\mathbb{C}}}}(\\det V)$" pointtype 13 linetype 15, \
     "cjv2t.csv" using 1:5 with points title "$\\mathop{\\mathrm{\\rho_{\\mathbb{C}}}}(\\cosh\\phi)$" pointtype 15 linetype -1, \
     "cjv2t.csv" using 1:7 with points title "$\\mathop{\\mathrm{\\rho_{\\mathbb{C}}}}(\\Im(\\mathrm{e}^{\\mathrm{i}\\beta}\\sinh\\phi))$" pointtype 5 linetype 4, \
     "cjv2t.csv" using 1:6 with points title "$\\mathop{\\mathrm{\\rho_{\\mathbb{C}}}}(\\Re(\\mathrm{e}^{\\mathrm{i}\\beta}\\sinh\\phi))$" pointtype 7 linetype -1
