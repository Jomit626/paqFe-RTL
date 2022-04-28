#!/bin/bash

set -e

PROJ_FOLDER="$(realpath $(dirname "$0")/..)"
PQAFE="$PROJ_FOLDER/build/paqFe/paqfe"
PQAFE_RTL="$PROJ_FOLDER/build/paqfe-rtl "

DATA_FOLDER=$PROJ_FOLDER/paqFe/test/data
TMP_FOLDER=$PROJ_FOLDER/tmp/driver/

ENWIK8_DATA=$PROJ_FOLDER/paqFe/test/big_data/enwik8
ENWIK8_DATA_COMPRESSED=$TMP_FOLDER/enwik8.paqfe-rtl

mkdir -p $TMP_FOLDER
echo Start compressing enwik8
sudo $PQAFE_RTL -i $ENWIK8_DATA -o $ENWIK8_DATA_COMPRESSED
origin_size=$(stat -c "%s" $ENWIK8_DATA)
compressed_size=$(stat -c "%s" $ENWIK8_DATA_COMPRESSED* | awk '{sum += $1} END{print sum}')
rate=$(echo "scale=6; $compressed_size / $origin_size * 100" | bc)
echo Compress Ratio $rate \%
echo Start decompressing enwik8

$PQAFE -x -i $ENWIK8_DATA_COMPRESSED -o $TMP_FOLDER/enwik8.bak
cmp $TMP_FOLDER/enwik8.bak $ENWIK8_DATA
echo enwik8 PASS!
