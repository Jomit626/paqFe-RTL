#!/bin/bash

set -e

PROJ_FOLDER="$(realpath $(dirname "$0")/..)"
PQAFE="$PROJ_FOLDER/paqFe/build/paqfe"
PQAFE_RTL="$PROJ_FOLDER/driver/paqfe-rtl "

DATA_FOLDER=$PROJ_FOLDER/paqFe/test/data
TMP_FOLDER=$PROJ_FOLDER/tmp/driver/

ENWIK8_DATA=$PROJ_FOLDER/paqFe/test/big_data/enwik8

mkdir -p $TMP_FOLDER

$PQAFE -c -i $ENWIK8_DATA -o $PROJ_FOLDER/paqFe/tmp/enwik8.paqfe
sudo $PQAFE_RTL -i $ENWIK8_DATA -o $TMP_FOLDER/enwik8.paqfe-rtl

for i in {0..7}
do
  cmp $TMP_FOLDER/enwik8.paqfe-rtl.$i $PROJ_FOLDER/paqFe/tmp/enwik8.paqfe.$i
done
echo enwik8 PASS!
