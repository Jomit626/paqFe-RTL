#!/bin/bash

set -e

PROJ_FOLDER="$(realpath $(dirname "$0")/..)"
PQAFE="$PROJ_FOLDER/paqFe/build/paqfe"
PQAFE_RTL="$PROJ_FOLDER/driver/paqfe-rtl "

DATA_FOLDER=$PROJ_FOLDER/paqFe/test/data
TMP_FOLDER=$PROJ_FOLDER/tmp/driver/

mkdir -p $TMP_FOLDER

echo -e file\\tresult\\torigin\\tcompressed\\trate
i=0
for file in $(ls $DATA_FOLDER)
do  
  # rm old files
  rm -f $TMP_FOLDER/$file.paqfe*

  # compress and decompress
  $PQAFE_RTL -i $DATA_FOLDER/$file -o $TMP_FOLDER/$file.paqfe > $TMP_FOLDER/$file.log
  $PQAFE -x -i $TMP_FOLDER/$file.paqfe -o $TMP_FOLDER/$file >/dev/null

  # test result
  diff $DATA_FOLDER/$file $TMP_FOLDER/$file  >/dev/null
  diff_res=$?

  # cal ratio
  origin_size=$(stat -c "%s" $DATA_FOLDER/$file)
  compressed_size=$(stat -c "%s" $TMP_FOLDER/$file.paqfe* | awk '{sum += $1} END{print sum}')
  rate=$(echo "scale=6; $compressed_size / $origin_size * 100" | bc)

  delta=$(echo "scale=6; ${LAST_RESULT[$i]} - $rate" | bc)

  echo -e $file\\t$diff_res\\t$origin_size\\t$compressed_size\\t$rate\%
  
  result="$result $rate"
  i=($i+1)
done
