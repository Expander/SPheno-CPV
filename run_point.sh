#!/bin/sh

point="$1"

source run_sg.sh

run_sg --sg=bin/SPheno                --point="$point" --pattern="mh_sp,h0"   --verbose=1
run_sg --sg=bin/SPhenoMSSMCPV         --point="$point" --pattern="mh_sa,hh_2" --verbose=1
run_sg --sg=bin/FlexibleSUSY-CMSSMCPV --point="$point" --pattern="mh_fs,hh(2" --verbose=1

echo ""
echo "=========================="
echo "Results for $point"
echo "=========================="

echo "SPheno       : $mh_sp"
echo "SPhenoMSSMCPV: $mh_sa"
echo "FlexibleSUSY : $mh_fs"
