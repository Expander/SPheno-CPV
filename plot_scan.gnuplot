set terminal pdf enhanced
set key box bottom
set xlabel "{/Symbol h}"

set output "plot_scan_mh1.pdf"
set ylabel "m_{h_1} / GeV"

plot "scan.dat" using 1:2 title "SPheno"        with linespoints, \
     "scan.dat" using 1:3 title "SPhenoMSSMCPV" with linespoints, \
     "scan.dat" using 1:4 title "FlexibleSUSY"  with linespoints

set output "plot_scan_mh2.pdf"
set ylabel "m_{h_2} / GeV"

plot "scan.dat" using 1:5 title "SPheno"        with linespoints, \
     "scan.dat" using 1:6 title "SPhenoMSSMCPV" with linespoints, \
     "scan.dat" using 1:7 title "FlexibleSUSY"  with linespoints

set output "plot_scan_mh3.pdf"
set ylabel "m_{h_3} / GeV"

plot "scan.dat" using 1:8  title "SPheno"        with linespoints, \
     "scan.dat" using 1:9  title "SPhenoMSSMCPV" with linespoints, \
     "scan.dat" using 1:10 title "FlexibleSUSY"  with linespoints
