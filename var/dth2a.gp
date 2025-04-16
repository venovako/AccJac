# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "dth2a.tex"
set key top right vertical spacing 2 width 8 box

set xrange [-28:0]
set xtics ("-27" -27, "-25" -25, "-23" -23, "-21" -21, "-19" -19, "-17" -17, "-15" -15, "-13" -13, "-11" -11, "-9" -9, "-7" -7, "-5" -5, "-3" -3, "-1" -1)
set xlabel "$\\lfloor\\lg|\\tanh(2\\phi)|\\rfloor$"

set yrange[0.75:2.00] # 0.5:4.0
set ylabel "$\\mathop{\\mathrm{average}}(\\mathop{\\rho_{\\mathrm{old}}}f_{\\phi})/\\mathop{\\mathrm{average}}(\\mathop{\\rho_{\\mathrm{new}}}f_{\\phi})$"
set ytics ("0.75" 0.75, "1.00" 1.00, "1.25" 1.25, "1.50" 1.50, "1.75" 1.75, "2.00" 2.00)

set datafile columnheaders
set label " $\\text{\\tiny$\\approx 3.9$}$" at -27,1.75 point pointtype 14
plot "dth2t.out" using 1:($9/$3) with points title "$f_{\\phi}=\\tanh\\phi$" pointtype 13 linetype 1, \
     "dth2t.out" using 1:($11/$5) with points title "$f_{\\phi}=\\cosh\\phi$" pointtype 15 linetype -1, \
     "dth2t.out" using 1:($13/$7) with points title "$f_{\\phi}=\\sinh\\phi$" pointtype 7 linetype 3
