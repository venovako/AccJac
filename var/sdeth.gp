# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "sdeth.tex"
set nokey
set xlabel "$|\\tanh\\theta|$"
set ylabel "$|1-(\\cosh^2\\theta-\\sinh^2\\theta)|/\\varepsilon_{32}$"
set datafile columnheaders
plot "sdeth.csv" using 1:2 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{det}}}}(\\tanh\\theta)$" pointtype 7 linetype -1 pointsize 0.25
