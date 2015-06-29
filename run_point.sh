#!/bin/sh

point="$1"

. ./run_sg.sh

run_sg --sg=bin/SPheno                --point="$point" --pattern="mh_sp,MASS[25]"    --verbose=1
run_sg --sg=bin/SPhenoMSSMCPV         --point="$point" --pattern="mh_sa,MASS[25]"    --verbose=1
run_sg --sg=bin/FlexibleSUSY-CMSSMCPV --point="$point" --pattern="mh_fscpv,MASS[25]" --verbose=1
run_sg --sg=bin/FlexibleSUSY-CMSSMCPC --point="$point" --pattern="mh_fscpc,MASS[25]" --verbose=1

echo ""
echo "=========================="
echo "Results for $point"
echo "=========================="

echo "SPheno          : $mh_sp"
echo "SPhenoMSSMCPV   : $mh_sa"
echo "FlexibleSUSY-CPV: $mh_fscpv"
echo "FlexibleSUSY-CPC: $mh_fscpc"
