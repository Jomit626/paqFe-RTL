#!/bin/bash

set -e
PROJ_FOLDER=`realpath $(dirname "$0")`

echo "******** Generating Verify Database ********"
$PROJ_FOLDER/src/test/resources/generate_verify_db.sh
echo "******** Generating Verify Database Done ********"
echo "******** Chisel Functional Test ********"
cd $PROJ_FOLDER
sbt 'runMain GetCompressorVerilog;test;'
cd -
echo "******** Chisel Functional Test Done ********"
echo "******** Vivado Functional Test ********"
$PROJ_FOLDER/sim/sim.sh
echo "******** Vivado Functional Test Done ********"
