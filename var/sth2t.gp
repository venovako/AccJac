# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "sth2t.tex"

set key top left vertical maxrows 3 spacing 1.75 width 3 box

set xrange [-26:0]
set xtics ("-25" -25, "-23" -23, "-21" -21, "-19" -19, "-17" -17, "-15" -15, "-13" -13, "-11" -11, "-9" -9, "-7" -7, "-5" -5, "-3" -3, "-1" -1)
set xlabel "$\\lfloor\\lg|\\tanh(2\\theta)|\\rfloor$"

set ylabel "maximal relative errors / $\\varepsilon_{32}$"

set datafile columnheaders

plot "sth2t.csv" using 1:8 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{new}}}}(\\sinh\\theta)$" pointtype 7 linetype 3, \
     "sth2t.csv" using 1:6 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{new}}}}(\\cosh\\theta)$" pointtype 15 linetype 1, \
     "sth2t.csv" using 1:4 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{new}}}}(\\tanh\\theta)$" pointtype 13 linetype 14, \
     "sth2t.csv" using 1:14 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{old}}}}(\\sinh\\theta)$" pointtype 7 linetype -1, \
     "sth2t.csv" using 1:12 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{old}}}}(\\cosh\\theta)$" pointtype 15 linetype 4, \
     "sth2t.csv" using 1:10 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{old}}}}(\\tanh\\theta)$" pointtype 13 linetype 15
     
