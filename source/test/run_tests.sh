#!/bin/sh
# Compile and run the Carik unit tests with instantfpc.
# Usage: ./run_tests.sh [runner options]   (default: -a)
#
# FastPlaz location: set FASTPLAZ_DIR to the FastPlaz folder (the one that
# contains src/ and test/). Default: ../../../vendors/fastplaz/FastPlaz (as in extra.cfg)
# SimpleAI location: SIMPLEAI_DIR, default ../../../vendors/SimpleAI

cd "$(dirname "$0")" || exit 1
HERE="$(pwd)"

FP="${FASTPLAZ_DIR:-$HERE/../../../vendors/fastplaz/FastPlaz}"
SA="${SIMPLEAI_DIR:-$HERE/../../../vendors/SimpleAI}/source/package"
CR="$HERE/.."
if [ ! -f "$FP/src/library/common.pas" ]; then
  echo "FastPlaz not found: $FP"
  echo "Set FASTPLAZ_DIR to the FastPlaz folder."
  exit 2
fi

# -B: always rebuild, the cache only tracks the main program source.
# Unit paths mirror source/carik/extra.cfg.
export INSTANTFPCOPTIONS="-B -Fi$FP -Fi$CR/carik/config -Fu$FP/test -Fu$FP -Fu$FP/src/helper -Fu$FP/src/systems -Fu$FP/src/library -Fu$FP/src/util -Fu$FP/src/model -Fu$FP/src/integration -Fu$FP/src/cms/controller -Fu$FP/src/cms/model -Fu$FP/src/cms/util -Fu$FP/src/vendor -Fu$FP/src/vendor/dcrypt -Fu$SA/src -Fu$SA/src/controller -Fu$CR/common"

echo "Compiling (instantfpc)..."
if [ $# -eq 0 ]; then
  set -- -a
fi
exec instantfpc carik_test.lpr "$@"
