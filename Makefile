root_dir ?= $(PWD)
regress_dir ?= $(root_dir)/regress
install_dir ?= $(root_dir)/utils/bin

SBT ?= sbt
SBT_FLAGS ?= -Dsbt.log.noformat=true

scala_jar ?= $(install_dir)/firrtl.jar
scala_src := $(shell find src -type f \( -name "*.scala" -o -path "*/resources/*" \))

build:	build-scala

clean:
	$(MAKE) -C $(root_dir)/spec clean
	rm -f $(install_dir)/firrtl.jar
	$(SBT) "clean"

.PHONY : specification
specification:
	$(MAKE) -C $(root_dir)/spec all

regress: $(scala_jar)
	cd $(regress_dir) && $(install_dir)/firrtl -i rocket.fir -o rocket.v -X verilog

# Scala Added Makefile commands

build-scala: $(scala_jar)

$(scala_jar): $(scala_src)
	$(SBT) "assembly"

test-scala:
	$(SBT) test

jenkins-build:	clean
	$(SBT) $(SBT_FLAGS) +clean +test +publish-local
	$(SBT) $(SBT_FLAGS) scalastyle coverage test
	$(SBT) $(SBT_FLAGS) coverageReport

RUN_CLASSPATH := runclasspath.txt

$(RUN_CLASSPATH): $(scala_src)
	sbt -error "export runtime:fullClasspath" > $@

NATIVE_IMAGE := native-image

GRAAL_ARGS := \
	--report-unsupported-elements-at-runtime \
	--class-path $(shell cat $(RUN_CLASSPATH)) \
	--initialize-at-build-time=com.google.protobuf.ExtensionRegistry

firrtl_native: $(RUN_CLASSPATH)
	$(NATIVE_IMAGE) $(GRAAL_ARGS) firrtl.Driver firrtl_native

.PHONY: build clean regress build-scala test-scala
