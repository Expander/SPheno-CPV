# [1] spectrum generator
# [2] SLHA input file
# [*] "variable,pattern" sets variable to the value with pattern in the output file
run_sg() {

    local sg=
    local point=
    local pattern=
    local verbose=1

    while test ! "x$1" = "x" ; do
        case "$1" in
            -*=*) optarg=`echo "$1" | sed 's/[-_a-zA-Z0-9]*=//'` ;;
            *) optarg= ;;
        esac

        case $1 in
            --sg=*)         sg=$optarg ;;
            --pattern=*)    pattern="$pattern $optarg" ;;
            --point=*)      point=$optarg ;;
            --verbose=*)    verbose=$optarg ;;
            *)  echo "run_sg: invalid option '$1'." ; exit 1 ;;
        esac
        shift
    done

    out="output/`basename $point | sed 's/\.in\./.out./'`"

    if test "$verbose" -gt "0"; then
        echo ">>> running $sg <<<"
        echo "    Input : $point"
        echo "    Output: $out"
    fi

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
        if test "$verbose" -gt "0"; then
            echo "    Error: program failed"
        fi

        set -- $pattern
        while test ! "x$1" = "x" ; do
            local var="`echo $1 | awk -F , '{print $1}'`"
            local pat="`echo $1 | awk -F , '{print $2}'`"
            eval "$var=0"
            shift
        done

        return 1
    fi

    set -- $pattern
    while test ! "x$1" = "x" ; do
        local var="`echo $1 | awk -F , '{print $1}'`"
        local pat="`echo $1 | awk -F , '{print $2}'`"
        local val="`grep $pat $out | awk '{print $2}'`"
        eval "$var=$val"
        shift
    done

    return 0
}
