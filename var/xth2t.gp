# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 12cm,6cm
set output "xth2t.tex"

set key top left vertical maxrows 2 spacing 1.75 width 3 box

set xrange [-34:0]
set xtics ("-33" -33, "-31" -31, "-29" -29, "-27" -27, "-25" -25, "-23" -23, "-21" -21, "-19" -19, "-17" -17, "-15" -15, "-13" -13, "-11" -11, "-9" -9, "-7" -7, "-5" -5, "-3" -3, "-1" -1)
set xlabel "$\\lfloor\\lg|\\tanh(2\\phi)|\\rfloor$"

set ylabel "maximal relative errors / $\\varepsilon$"

set datafile columnheaders

plot "xth2t.out" using 1:4 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{new}}}}(\\tanh\\phi)$" pointtype 13 linetype 14, \
     "xth2t.out" using 1:6 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{new}}}}(\\cosh\\phi)$" pointtype 15 linetype 1, \
     "xth2t.out" using 1:8 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{new}}}}(\\sinh\\phi)$" pointtype 7 linetype 3, \
     "xth2t.out" using 1:10 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{old}}}}(\\tanh\\phi)$" pointtype 13 linetype 15, \
     "xth2t.out" using 1:12 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{old}}}}(\\cosh\\phi)$" pointtype 15 linetype 4, \
     "xth2t.out" using 1:14 with points title "$\\mathop{\\mathrm{\\rho_{\\mathrm{old}}}}(\\sinh\\phi)$" pointtype 7 linetype -1
