root_dir ?= $(PWD)
regress_dir ?= $(root_dir)/regress
install_dir ?= $(root_dir)/utils/bin

SBT ?= sbt
scala_jar ?= $(install_dir)/firrtl.jar
scala_src := $(shell find src -type f \( -name "*.scala" -o -path "*/resources/*" \))

clean:
	rm -f $(install_dir)/firrtl.jar
	$(SBT) "clean"

build:	build-scala

regress: $(scala_jar)
	cd $(regress_dir) && $(install_dir)/firrtl -i rocket.fir -o rocket.v -X verilog

# Scala Added Makefile commands

build-scala: $(scala_jar)

$(scala_jar): $(scala_src)
	$(SBT) "assembly"

test-scala:
	$(SBT) test

.PHONY: build clean regress build-scala test-scala
