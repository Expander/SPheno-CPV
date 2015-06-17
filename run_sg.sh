# [1] spectrum generator
# [2] SLHA input file
# [*] "variable,pattern" sets variable to the value with pattern in the output file
run_sg() {
    local sg="$1"
    shift
    local point="$1"
    shift

    out="output/`basename $point | sed 's/\.in\./.out./'`"

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
