#!/bin/bash

set -e

PROJ_FOLDER="$(realpath $(dirname "$0")/..)"

DATA_FOLDER=$PROJ_FOLDER/paqFe/verify/db
TMP_FOLDER=$PROJ_FOLDER/tmp/driver/

mkdir -p $TMP_FOLDER

for line in $(cat $DATA_FOLDER/all)
do
  line=(${line//,/ })

  test_name=${line[0]}
  input=${line[1]}
  output=${line[2]}
  rm -f $TMP_FOLDER/$test_name*
  sudo $PROJ_FOLDER/driver/paqfe-rtl -i $input -o $TMP_FOLDER/$test_name

  for i in {0..7}
  do
    cmp $TMP_FOLDER/$test_name.$i $output.$i
  done
  echo $test_name PASS!
done
