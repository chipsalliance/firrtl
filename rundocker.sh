#!/bin/env bash
set -e

docker run -it --net=host \
  -e TRAVIS_SCALA_VERSION=$TRAVIS_SCALA_VERSION \
  -e TRAVIS_BUILD_DIR=$TRAVIS_BUILD_DIR \
  -v $HOME/.ivy2:/root/.ivy2 \
  -v $HOME/.coursier:/root/.coursier \
  -v $HOME/.sbt:/root/.sbt \
  -v $TRAVIS_BUILD_DIR:/firrtl \
  tampler/chisel-crew-base:11 bash -c "
  set -e # exit on failure
  set -x # trace for debug
  cd /firrtl
  java --version
  verilator --version
  yosys -V
  chmod 777 .run_chisel_tests.sh
  chmod 777 .run_formal_checks.sh
  ./sbt -sbt-launch-dir ./launchers/ ++$TRAVIS_SCALA_VERSION \"unidoc;\"
  ./.run_chisel_tests.sh
  ./.run_formal_checks.sh Ops"
