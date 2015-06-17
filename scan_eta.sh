#!/bin/sh

start="0"
stop="3.141"
steps=20

source run_sg.sh

for i in `seq 0 $steps`
do
    eta=$(cat <<EOF | bc
scale=10
$start + ($stop - $start)*${i} / $steps
EOF
    )

    # create point
    point="input/point.in.slha"
    cat templates/FlexibleSUSY.tmpl >  $point
    cat templates/SPheno.tmpl       >> $point

    cat <<EOF >> $point
Block MODSEL            # Select model
 1    1                 # mSugra
 5    2                 # full CP violation
Block SMINPUTS          # Standard Model inputs
 2   1.166379E-05       # G_F, Fermi constant
 3   1.184000E-01       # alpha_s(MZ) SM MSbar
 4   9.118760E+01       # Z-boson pole mass
 5   4.180000E+00       # m_b(mb) SM MSbar
 6   1.731000E+02       # m_top(pole)
 7   1.776820E+00       # m_tau(pole)
Block MINPAR                 # Input parameters
 1   7.000000E+01       # m0
 2   2.500000E+02       # m12
 3   1.000000E+01       # tanb
 4   1                  # sign(mu)
 5  -3.000000E+02       # A0
Block EXTPAR
  100   $eta            # etaInput
EOF


    run_sg --sg=bin/SPheno                --point="$point" --pattern="mh_sp,h0"   --verbose=0
    run_sg --sg=bin/SPhenoMSSMCPV         --point="$point" --pattern="mh_sa,hh_2" --verbose=0
    run_sg --sg=bin/FlexibleSUSY-CMSSMCPV --point="$point" --pattern="mh_fs,hh(2" --verbose=0

    echo ""
    echo "eta = $eta"
    echo "SPheno       : $mh_sp"
    echo "SPhenoMSSMCPV: $mh_sa"
    echo "FlexibleSUSY : $mh_fs"
done
