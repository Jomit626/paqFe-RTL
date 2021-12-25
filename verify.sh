#!/bin/bash

set -e
PROJ_FOLDER="$(dirname "$0")"

if ! [ -d $PROJ_FOLDER/paqFe/verify/db ] ; then
  $PROJ_FOLDER/src/test/resources/generate_verify_db.sh
fi

cd $PROJ_FOLDER
sbt 'runMain GetCompressorVerilog;test;'
cd -

$PROJ_FOLDER/sim/sim.sh

