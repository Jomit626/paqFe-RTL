#!/bin/bash

set -e
PROJ_FOLDER="$(dirname "$0")/.."

cd $PROJ_FOLDER/sim

mkdir -p tmp

for line in $(cat ../paqFe/verify/db/all)
do
  line=(${line//,/ })

  test_name=${line[0]}
  input=${line[1]}
  output=${line[2]}

  # we need rand value in reg at beginning so that vivado xsim would no assign they to X.
  xelab -a \
        -d INPUT_FILE=\"$input\" \
        -d OUTPUT_FILE=\"tmp/$test_name\" \
        -d RANDOMIZE_REG_INIT=1 \
        -prj sim.proj -s sim default.tb \
        --debug typical --nolog -O3
  ./axsim.sh

  for i in {0..7}
  do
    diff -s tmp/$test_name.$i $output.$i
  done
done

rm -f ./axsim.sh
rm -f *.log
rm -f *.jou
rm -f *.pb
rm -rf xsim.dir

cd -