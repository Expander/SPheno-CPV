set terminal pdf enhanced
set key box bottom
set xlabel "{/Symbol h}"

set output "plot_scan_mh1.pdf"
set ylabel "m_{h_1} / GeV"

plot "scan.dat" using 1:2  title "SPheno"           with linespoints, \
     "scan.dat" using 1:14 title "SPhenoMSSM"       with linespoints, \
     "scan.dat" using 1:5  title "SPhenoMSSMCPV"    with linespoints, \
     "scan.dat" using 1:8  title "FlexibleSUSY-CPV" with linespoints, \
     "scan.dat" using 1:11 title "FlexibleSUSY-CPC" with linespoints

set output "plot_scan_mh2.pdf"
set ylabel "m_{h_2} / GeV"

plot "scan.dat" using 1:3  title "SPheno"           with linespoints, \
     "scan.dat" using 1:15 title "SPhenoMSSM"       with linespoints, \
     "scan.dat" using 1:6  title "SPhenoMSSMCPV"    with linespoints, \
     "scan.dat" using 1:9  title "FlexibleSUSY-CPV" with linespoints, \
     "scan.dat" using 1:13 title "FlexibleSUSY-CPC" with linespoints

set output "plot_scan_mh3.pdf"
set ylabel "m_{h_3} / GeV"

plot "scan.dat" using 1:4  title "SPheno"           with linespoints, \
     "scan.dat" using 1:16 title "SPhenoMSSM"       with linespoints, \
     "scan.dat" using 1:7  title "SPhenoMSSMCPV"    with linespoints, \
     "scan.dat" using 1:10 title "FlexibleSUSY-CPV" with linespoints, \
     "scan.dat" using 1:12 title "FlexibleSUSY-CPC" with linespoints
