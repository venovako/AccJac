# max 140 (W) x 195 (H) mm
set term cairolatex pdf standalone header "\\usepackage{amsmath,amsfonts}" color font ", 10" size 10cm,6cm
set output "resid.tex"

set key bottom right vertical spacing 1.75
set logscale x 2
set logscale y 10

set xrange [1:8192]
set xlabel "$k$ (matrix order $n=2^k$)"
set xtics ("1" 2, "2" 4, "3" 8, "4" 16, "5" 32, "6" 64, "7" 128, "8" 256, "9" 512, "10" 1024, "11" 2048, "12" 4096)
set ylabel "relative residuals"
set yrange [1e-16:1.00001e-10]
set ytics ("$10^{-16}$" 1e-16, "$10^{-15}$" 1e-15, "$10^{-14}$" 1e-14, "$10^{-13}$" 1e-13, "$10^{-12}$" 1e-12, "$10^{-11}$" 1e-11, "$10^{-10}$" 1e-10)

set datafile columnheaders
set multiplot
plot "z-2.csv" using 1:7 with points title "$\\max_j^{\\mathtt{rc}}|\\lambda_j^{}-\\sigma_j^2J_{jj}^{}|/|\\lambda_j^{}|\\mskip-15mu\\null$" pointtype 15 linetype 1, \
     "z-0.csv" using 1:7 with points title "$\\max_j^{\\mathtt{dR}}|\\lambda_j^{}-\\sigma_j^2J_{jj}^{}|/|\\lambda_j^{}|\\mskip-15mu\\null$" pointtype 13 linetype 7
set key top left vertical spacing 1.75 width -11
plot "z-2.csv" using 1:11 with points title "$\\max_j^{\\mathtt{rc}}\\|Gv_j^{}-u_j^{}\\sigma_j^{}\\|_F^{}/\\sigma_j^{}\\mskip-15mu\\null$" pointtype 5 linetype -1, \
     "z-0.csv" using 1:11 with points title "$\\max_j^{\\mathtt{dR}}\\|Gv_j^{}-u_j^{}\\sigma_j^{}\\|_F^{}/\\sigma_j^{}\\mskip-15mu\\null$" pointtype 7 linetype 2, \
     "z-2.csv" using 1:8 with points title "$\\|G-U_{\\mathtt{rc}}^{}\\Sigma_{\\mathtt{rc}}^{}V_{\\mathtt{rc}}^{-1}\\|_F^{}/\\|G\\|_F^{}\\mskip-15mu\\null$" pointtype 11 linetype 4, \
     "z-0.csv" using 1:8 with points title "$\\|G-U_{\\mathtt{dR}}^{}\\Sigma_{\\mathtt{dR}}^{}V_{\\mathtt{dR}}^{-1}\\|_F^{}/\\|G\\|_F^{}\\mskip-15mu\\null$" pointtype 9 linetype 6
unset multiplot
