#!/bin/sh

# Testing script for Blox, adapted from microc testing suite
# Step through a list of files
# Compile, run, and check the output of each expected-to-work test
# Compile and check the error of each expected-to-fail test

# Path to the LLVM interpreter
LLI="lli"

# Path to the Blox compiler.
# Try "_build/blox.native" if ocamlbuild was unable to create a symbolic link.
BLOX="./blox.native"

# Set time limit for all operations
ulimit -t 30

globallog=testsuite.log
rm -f $globallog
rm -rf *.diff *.out *.ll *.err 

fail="FAIL"
pass="PASS"
one=1

error=0
keep=0
globalerror=0
totalfiles=0
totalerrors=0

GREEN='\033[1;32m'
BLUE='\033[0;34m'
RED='\033[1;31m'
NC='\033[0m'

Usage() 
{
    echo "\nUsage: <testsuite.sh> [options] <.blox files>"
    echo "Options:"
    echo "\t-k    Keep intermediate files"
    echo "\t-h    Display help menu"
    exit 1
}

SignalError() 
{
    if [ $error -eq 0 ] ; then
	   printf "${RED}%6s${NC}" $fail
	   error=1
       totalerrors=$(($totalerrors + $one))
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile, with any differences written to difffile
Compare() 
{
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || 
    {
	   SignalError "$1 and $2 have different output:"
       cat $1
       
	   echo "==> $1 and $2 have different output" 1>&2
       
       
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() 
{
    echo $* 1>&2
    eval $* || 
    {
	   SignalError "on $*"
	   return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() 
{
    echo $* 1>&2
    eval $* && 
    {
        SignalError "failed: $* did not report an error"
        return 1
    }
    return 0
}

ReportStatus()
{
    # Report the status and clean up the generated files
    if [ $error -eq 0 ] ; then
        if [ $keep -eq 0 ] ; then
            rm -f $generatedfiles
        fi
        printf "${GREEN}%6s${NC}\n" $pass
        echo "###### PASS ######\n" 1>&2
    else
        # printf "${RED}%7s${NC}\n" $fail
        echo "###### FAIL ######\n" 1>&2
        globalerror=$error
    fi
}

Check() 
{
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.blox//'`
    reffile=`echo  $1 | sed 's/.blox$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
    
    printf "%18s" $basename
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""
    generatedfiles="$generatedfiles ${basename}.ll ${basename}.out" &&
    Run "$BLOX" "<" $1 ">" "${basename}.ll" &&
    Run "$LLI" "${basename}.ll" ">" "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff

    ReportStatus
}

CheckFail() 
{
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.blox//'`
    reffile=`echo  $1 | sed 's/.blox$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    printf "%18s" $basename
    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""
    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    RunFail "$BLOX" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
    Compare ${basename}.err ${reffile}.err ${basename}.diff

    ReportStatus
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

# exit $globalerror
