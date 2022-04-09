#!/bin/bash

set -e

PROJ_FOLDER="$(realpath $(dirname "$0")/..)"

DATA_FOLDER=$PROJ_FOLDER/paqFe/verify/db
TMP_FOLDER=$PROJ_FOLDER/tmp/driver/

mkdir -p $TMP_FOLDER
SIZE=1M

dd if=$PROJ_FOLDER/paqFe/test/big_data/enwik8 of=$TMP_FOLDER/sized_file bs=$SIZE count=1
sudo $PROJ_FOLDER/driver/paqfe-rtl -i $TMP_FOLDER/sized_file -o $TMP_FOLDER/sized_file.paqfe

