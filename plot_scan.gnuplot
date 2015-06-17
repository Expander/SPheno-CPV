set terminal pdf enhanced

set output "plot_scan.pdf"
set key box bottom
set xlabel "{/Symbol h}"
set ylabel "m_{h_1} / GeV"

plot "scan.dat" using 1:2 title "SPheno"        with linespoints, \
     "scan.dat" using 1:3 title "SPhenoMSSMCPV" with linespoints, \
     "scan.dat" using 1:4 title "FlexibleSUSY"  with linespoints
