#!/bin/sh

point="$1"

run_sg() {
    local sg="$1"
    shift
    local point="$1"
    shift

    out="output/`basename $sg`.out.`basename $point | sed 's/LesHouches\.in\.//'`"

    echo ">>> running $sg <<<"
    echo "    Input : $point"
    echo "    Output: $out"

    case "`basename $sg`" in
        SPheno*)       cmd="$sg $point $out" ;;
        FlexibleSUSY*) cmd="$sg --slha-input-file=$point --slha-output-file=$out" ;;
        *)             echo "Error: unknown spectrum generator type: $sg";
                       cmd="false" ;;
    esac

    rm -f "$out"
    $cmd > /dev/null 2>&1
    local exit_code="$?"

    if test "$exit_code" != "0" -o ! -e "$out"; then
        echo "    Error: program failed"

        while test ! "x$1" = "x" ; do
            local var="`echo $1 | awk -F , '{print $1}'`"
            local pat="`echo $1 | awk -F , '{print $2}'`"
            eval "$var=0"
            shift
        done

        return 1
    fi

    while test ! "x$1" = "x" ; do
        local var="`echo $1 | awk -F , '{print $1}'`"
        local pat="`echo $1 | awk -F , '{print $2}'`"
        local val="`grep $pat $out | awk '{print $2}'`"
        eval "$var=$val"
        shift
    done

    return 0
}

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
