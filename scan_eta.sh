#!/bin/sh

start="0"
stop="6.28319"
steps=30
printf_pattern="%12s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s %20s\n"

. ./run_sg.sh

printf "# $printf_pattern" "eta" \
    "h1(SPheno)"           "h2(SPheno)"           "h3(SPheno)"           \
    "h1(SPhenoMSSMCPV)"    "h2(SPhenoMSSMCPV)"    "h3(SPhenoMSSMCPV)"    \
    "h1(FlexibleSUSY-CPV)" "h2(FlexibleSUSY-CPV)" "h3(FlexibleSUSY-CPV)" \
    "h1(FlexibleSUSY-CPC)" "h2(FlexibleSUSY-CPC)" "a2(FlexibleSUSY-CPC)" \
    "h1(SPhenoMSSM)"       "h2(SPhenoMSSM)"       "a2(SPhenoMSSM)"

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
 2   1.206379E-05       # G_F, Fermi constant
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
 6   $eta               # etaInput (for SPheno)
Block EXTPAR
  100   $eta            # etaInput (for FlexibleSUSY)
EOF

    run_sg --sg=bin/SPheno                --point="$point" --pattern="mh1_sp,h0"    --pattern="mh2_sp,H0"    --pattern="mh3_sp,A0"   --verbose=0
    run_sg --sg=bin/SPhenoMSSMCPV         --point="$point" --pattern="mh1_sa,hh_2"  --pattern="mh2_sa,hh_3"  --pattern="mh3_sa,hh_4" --verbose=0
    run_sg --sg=bin/SPhenoMSSM            --point="$point" --pattern="mh1_sac,hh_1" --pattern="mh2_sac,hh_2" --pattern="ma2_sac,Ah_2" --verbose=0
    run_sg --sg=bin/FlexibleSUSY-CMSSMCPV --point="$point" --pattern="mh1_fs,hh(2"  --pattern="mh2_fs,hh(3"  --pattern="mh3_fs,hh(4" --verbose=0
    run_sg --sg=bin/FlexibleSUSY-CMSSM    --point="$point" --pattern="mh1_fsc,hh(1" --pattern="mh2_fsc,hh(2" --pattern="ma2_fsc,Ah(2" --verbose=0

    printf "  $printf_pattern" "$eta" \
        "$mh1_sp"  "$mh2_sp"  "$mh3_sp" \
        "$mh1_sa"  "$mh2_sa"  "$mh3_sa" \
        "$mh1_fs"  "$mh2_fs"  "$mh3_fs" \
        "$mh1_fsc" "$mh2_fsc" "$ma2_fsc" \
        "$mh1_sac" "$mh2_sac" "$ma2_sac"

done
