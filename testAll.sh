#!/bin/sh

# Testing script for Blox, adapted from microc testing suite
# Step through a list of files
# Compile, run, and check the output of each expected-to-work test
# Compile and check the error of each expected-to-fail test

# Set time limit for all operations
ulimit -t 30

globallog=testAll.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: testAll.sh [options] [.blox files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	   SignalError "$1 $2 differs"
	   echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	   SignalError "$1 failed on $*"
	   return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
        SignalError "failed: $* did not report an error"
        return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.blox//'`
    reffile=`echo $1 | sed 's/.blox$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

# Not sure about how to do this part
#    generatedfiles="$generatedfiles ${basename}.i.out" &&
#    Run "$.blox"  "<" $1 ">" ${basename}.i.out &&
#    Compare ${basename}.i.out ${reffile}.out ${basename}.i.diff

#    generatedfiles="$generatedfiles ${basename}.ll ${basename}.out" &&
#    Run "$MICROC" "<" $1 ">" "${basename}.ll" &&
#    Compare ${basename}.out ${reffile}.out ${basename}.diff

#    generatedfiles="$generatedfiles ${basename}.c.out" &&
#    Run "$MICROC"  "<" $1 ">" ${basename}.c.out  &&
#    Compare ${basename}.c.out ${reffile}.out ${basename}.c.diff



    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.mc//'`
    reffile=`echo $1 | sed 's/.mc$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

# Again not sure what to do here
#    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
#    RunFail "$MICROC" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
#    Compare ${basename}.err ${reffile}.err ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
        if [ $keep -eq 0 ] ; then
            rm -f $generatedfiles
        fi
        echo "OK"
        echo "###### SUCCESS" 1>&2
    else
        echo "###### FAILED" 1>&2
        globalerror=$error
    fi
}


while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.blox tests/fail-*.blox"
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror
