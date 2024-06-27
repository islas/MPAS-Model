#!/bin/sh
help()
{
  echo "./build.sh [workingdir] [options] [-- <hostenv.sh options>]"
  echo "  [workingdir]              First argument must be the working dir to immediate cd to"
  echo "  -b                        Additional make arguments passed in"
  echo "  -c                        Core to build"
  echo "  -t                        Target configuration (gnu, intel, etc)"
  echo "  -d                        Debug build"
  echo "  -e                        environment variables in comma-delimited list, e.g. var=1,foo,bar=0"
  echo "  -- <hostenv.sh options>   Directly pass options to hostenv.sh, equivalent to hostenv.sh <options>"
  echo "  -h                  Print this message"
  echo ""
  echo "If you wish to use an env var in your arg such as '-b core=\$CORE -e CORE=atmosphere', you must"
  echo "you will need to do '-b \\\$CORE -e CORE=atmosphere' to delay shell expansion"
}

echo "Input arguments:"
echo "$*"

workingDirectory=$1
shift
if [ $workingDirectory = "-h" ]; then
  help
  exit 0
fi
cd $workingDirectory

# Get some helper functions
. .ci/env/helpers.sh

while getopts b:c:t:de:h opt; do
  case $opt in
    b)
      buildCommand="$OPTARG"
    ;;
    c)
      core="$OPTARG"
    ;;
    t)
      target="$OPTARG"
    ;;
    d)
      debug="DEBUG=true"
    ;;
    e)
      envVars="$envVars,$OPTARG"
    ;;
    h)  help; exit 0 ;;
    *)  help; exit 1 ;;
    :)  help; exit 1 ;;
    \?) help; exit 1 ;;
  esac
done

shift "$((OPTIND - 1))"

# Everything else goes to our env setup
. .ci/env/hostenv.sh $*

# Now evaluate env vars in case it pulls from hostenv.sh
if [ ! -z "$envVars" ]; then
  setenvStr "$envVars"
fi

# Re-evaluate input values for delayed expansion
eval "core=\"$core\""
eval "target=\"$target\""
eval "buildCommand=\"$buildCommand\""

make clean CORE=$core

echo "Compiling with options $target core=$core $debug $buildCommand"
echo "make $target CORE=$core $configuration $debug $buildCommand"
make $target CORE=$core $configuration $debug $buildCommand
result=$?

if [ $result -ne 0 ]; then
  echo "Failed to compile"
  exit 1
fi

echo "TEST $(basename $0) PASS"
