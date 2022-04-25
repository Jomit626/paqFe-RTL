#!/bin/bash

set -e
PROJ_FOLDER="$(realpath $(dirname "$0")/..)"

cd $PROJ_FOLDER/sim

mkdir -p tmp

echo Compiling module
# we need rand value in reg at beginning so that vivado xsim would no assign them to X.
xelab -a \
      -d PRINTF_COND=1 \
      -d RANDOMIZE_REG_INIT=1 \
      -d RANDOMIZE_MEM_INIT=1 \
      -prj sim.proj -s sim default.tb \
      --nolog -O3
echo Start Sim

for line in $(cat $PROJ_FOLDER/verify/db/all)
do
  line=(${line//,/ })

  test_name=${line[0]}
  input=${line[1]}
  output=${line[2]}

  ./axsim.sh \
    --testplusarg inputfile=$input \
    --testplusarg outputfile=tmp/$test_name
  
  for i in {0..7}
  do
    cmp tmp/$test_name.$i $output.$i
  done
  echo $test_name PASS!
done

rm -f ./axsim.sh *.log *.jou *.pb *.str
rm -rf xsim.dir .Xil

cd -