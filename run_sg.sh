# [1] spectrum generator
# [2] SLHA input file
# [*] "variable,pattern" sets variable to the value with pattern in the output file

run_sg() {
    # prints SLHA block
    local print_slha_block_awk='
BEGIN {
   is_block = 0;
   if (block == "") {
      print "Error: block name not defined";
      print "   Please define the block name with -v block=<block-name>";
      exit 1
   }
}
{
   pattern     = "^block[[:blank:]]*" tolower(block) "([^[:graph:]].*)?$";
   not_pattern = "^block[[:blank:]]*.*$";

   if (tolower($0) ~ pattern) {
      is_block = 1
   } else if (tolower($0) ~ not_pattern) {
      is_block = 0
   };

   if (is_block)
      print $0
}
'

    # prints block entry
    # expects block entry keys in the form x or x:y or x:y:z etc.
    local print_block_entry_awk='
{
  split(keys,k,":");

  matches = 1;

  for (i in k) {
     if ($(i) != k[i])
        matches = 0
  }

  if (matches == 1)
     print $(length(k)+1)
}
'

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
            eval "$var=0"
            shift
        done

        return 1
    fi

    set -- $pattern
    while test ! "x$1" = "x" ; do
        local var="`echo $1 | awk -F , '{print $1}'`"
        local pat="`echo $1 | awk -F , '{print $2}'`"

        local output_block=$(echo "$pat" | awk -F [ '{ print $1 }')
        local full_block="$(cat $out | awk -v block="$output_block" "$print_slha_block_awk")"
        local block_entries="$(echo "$pat" | awk -F '[][]' '{ print $2 }')"
        local val="$(echo "$full_block" | awk -v keys="$block_entries" "$print_block_entry_awk")"

        eval "$var=$val"
        shift
    done

    return 0
}
