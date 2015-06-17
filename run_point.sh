#!/bin/sh

point="$1"

source run_sg.sh

run_sg bin/SPheno        "$point" "mh_sp,h0"
run_sg bin/SPhenoMSSMCPV "$point" "mh_sa,hh_2"
run_sg bin/FlexibleSUSY-CMSSMCPV "$point" "mh_fs,hh(2"

echo ""
echo "=========================="
echo "Results for $point"
echo "=========================="

echo "SPheno       : $mh_sp"
echo "SPhenoMSSMCPV: $mh_sa"
echo "FlexibleSUSY : $mh_fs"
