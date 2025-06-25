# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "sdeth.tex"
set nokey
set xlabel "$|\\tanh\\theta|$"
set ylabel "$|1-(\\cosh^2\\theta-\\sinh^2\\theta)|/\\varepsilon_{32}$"
set xrange [0.79969442:0.8]
set xtics ("0.7997" 0.7997, "0.7998" 0.7998, "0.7999" 0.7999, "0.8" 0.8)
set yrange [0.0:4.5]
set ytics ("0.0" 0.0, "0.5" 0.5, "1.0" 1.0, "1.5" 1.5, "2.0" 2.0, "2.5" 2.5, "3.0" 3.0, "3.5" 3.5, "4.0" 4.0, "4.5" 4.5)
set datafile columnheaders
plot "sdeth.csv" using 1:2 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{det}}}}(\\tanh\\theta)$" pointtype 7 linetype -1 pointsize 0.25
