#!/bin/bash

set -e
PROJ_FOLDER="$(dirname "$0")/../../.."

if ! [ -d $PROJ_FOLDER/paqFe/test/data ] ; then
  $PROJ_FOLDER/paqFe/test/download-data.sh
fi

mkdir -p $PROJ_FOLDER/paqFe/build
cd $PROJ_FOLDER/paqFe/build

cmake ..
make 
make verify-db

cd -