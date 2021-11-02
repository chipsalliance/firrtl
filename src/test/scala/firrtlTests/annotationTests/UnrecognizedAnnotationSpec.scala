// SPDX-License-Identifier: Apache-2.0

package firrtlTests.annotationTests

import firrtl.annotations._
import firrtl.passes.memlib.ReplSeqMemAnnotation
import firrtl.stage.FirrtlMain
import firrtl.testutils.FirrtlFlatSpec
import firrtl.transforms.BlackBoxInlineAnno
import logger.Logger
import logger.Logger.OutputCaptor

import java.io.{File, PrintWriter}

class UnrecognizedAnnotationSpec extends FirrtlFlatSpec {
  behavior.of("unrecognized annotations can be carried through serialization and deserialization")

  it should "preserve unknown annotations when allowed" in {
    val annotations =
      JsonProtocol.deserialize(UnrecognizedAnnotationTextGenerator.jsonText(includeAllowUnrecognizedAnnotations = true))

    annotations.exists(_.isInstanceOf[BlackBoxInlineAnno]) should be(true)
    annotations.count(_.isInstanceOf[UnrecognizedAnnotation]) should be(2)
    annotations.exists(_.isInstanceOf[ReplSeqMemAnnotation]) should be(false)

    val jsonOutputText = JsonProtocol.serialize(annotations)

    jsonOutputText should include(""""class":"firrtl.transforms.BlackBoxInlineAnno"""")
    jsonOutputText should include(""""class":"freechips.rocketchip.util.RegFieldDescMappingAnnotation"""")
    jsonOutputText should include(""""class":"freechips.rocketchip.util.SRAMAnnotation"""")
  }

  it should "throw an error when unknown annotations are present but AllowUnrecognizedAnnotation is not" in {

    // Default log level is error, which the JSON parsing uses here
    Logger.makeScope(Seq()) {
      val captor = new OutputCaptor
      Logger.setOutput(captor.printStream)

      val parsingError = intercept[UnrecogizedAnnotationsException] {
        JsonProtocol.deserialize(
          UnrecognizedAnnotationTextGenerator.jsonText(includeAllowUnrecognizedAnnotations = false)
        )
      }
      parsingError.getMessage should include("RegFieldDescMappingAnnotation")

      val output = captor.getOutputAsString
      output should include("Annotation parsing found unrecognized annotations")
      output should include(
        "This error can be ignored with an AllowUnrecognizedAnnotationsAnnotation or command line flag --allow-unrecognized-annotations"
      )
      output should include(
        "freechips.rocketchip.util.RegFieldDescMappingAnnotation"
      )
      output should include(
        "freechips.rocketchip.util.SRAMAnnotation"
      )
    }
  }

  case class TestFileNames(
    allowUnrecognized: Boolean,
    inputAnnotations:  String,
    outputAnnotations: String,
    firrtlSource:      String,
    firrtlOutput:      String)

  def setupFiles(allowUnrecognized: Boolean): TestFileNames = {
    val dirName = if (allowUnrecognized) {
      s"test_run_dir/unrecognized_annotation_allowed"
    } else {
      s"test_run_dir/unrecognized_annotation_not_allowed"
    }
    val dir = new File(dirName)
    dir.mkdirs()

    val fileNames = TestFileNames(
      allowUnrecognized = allowUnrecognized,
      inputAnnotations = s"$dirName/input_annotations.json",
      outputAnnotations = s"$dirName/output_annotations.json",
      firrtlSource = s"$dirName/trivial.fir",
      firrtlOutput = s"$dirName/trivial_out"
    )

    def writeText(fileName: String, text: String): Unit = {
      val writer = new PrintWriter(fileName)
      writer.write(text)
      writer.close()
    }

    writeText(
      fileNames.inputAnnotations,
      UnrecognizedAnnotationTextGenerator.jsonText(includeAllowUnrecognizedAnnotations = allowUnrecognized)
    )
    writeText(
      fileNames.firrtlSource,
      s"""
         |circuit Trivial :
         |  module Trivial :
         |    input clock : Clock
         |    input reset : UInt<1>
         |""".stripMargin
    )
    fileNames
  }

  /* construct an array of command line strings, based on file names and  */
  def makeCommandLineArgs(fileNames: TestFileNames): Array[String] = {

    (if (fileNames.allowUnrecognized) {
       Array("--allow-unrecognized-annotations")
     } else {
       Array.empty[String]
     }) ++
      Array(
        "--annotation-file",
        fileNames.inputAnnotations,
        "-i",
        fileNames.firrtlSource,
        "-X",
        "high",
        "-o",
        fileNames.firrtlOutput,
        "--output-annotation-file",
        fileNames.outputAnnotations
      )
  }

  it should "fail when command line annotation file contains unrecognized annotations and they are not allowed" in {
    val fileNames = setupFiles(allowUnrecognized = false)
    val args = makeCommandLineArgs(fileNames)
    val e = intercept[InvalidAnnotationFileException] {
      FirrtlMain.main(args)
    }

    e.getMessage should include(fileNames.inputAnnotations)
    e.getCause.getMessage should include("freechips.rocketchip.util.RegFieldDescMappingAnnotation")
    e.getCause.getMessage should include("freechips.rocketchip.util.SRAMAnnotation")
  }

  it should "not fail when command line annotation file contains unrecognized annotations but AllowUnrecognized " in {
    val fileNames = setupFiles(allowUnrecognized = true)
    val args = makeCommandLineArgs(fileNames)
    FirrtlMain.main(args)
  }
}

object UnrecognizedAnnotationTextGenerator {

  def jsonText(includeAllowUnrecognizedAnnotations: Boolean): String = {
    val serializedAllowUnrecognized = if (includeAllowUnrecognizedAnnotations) {
      """
        |  {
        |    "class": "firrtl.stage.AllowUnrecognizedAnnotations$"
        |  },""".stripMargin
    } else {
      ""
    }

    s"""|[$serializedAllowUnrecognized
        |  {
        |    "class": "firrtl.transforms.BlackBoxInlineAnno",
        |    "target": "TestHarness.plusarg_reader_27",
        |    "name": "plusarg_reader.v",
        |    "text": "License text"
        |  },
        |  {
        |    "class": "freechips.rocketchip.util.RegFieldDescMappingAnnotation",
        |    "target": "TestHarness.PeripheryBus",
        |    "regMappingSer": {
        |      "displayName": "PeripheryBus",
        |      "deviceName": "PeripheryBus",
        |      "baseAddress": 16384,
        |      "regFields": [
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 0,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_0",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        },
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 8,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_8",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        },
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 16,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_16",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        },
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 24,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_24",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        },
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 32,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_32",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        },
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 40,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_40",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        },
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 48,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_48",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        },
        |        {
        |          "byteOffset": "0x0",
        |          "bitOffset": 56,
        |          "bitWidth": 8,
        |          "name": "unnamedRegField0_56",
        |          "resetValue": 0,
        |          "accessType": "None",
        |          "wrType": "None",
        |          "rdAction": "None",
        |          "desc": "None",
        |          "group": "None",
        |          "groupDesc": "None",
        |          "volatile": false,
        |          "hasReset": false,
        |          "enumerations": {}
        |        }
        |      ]
        |    }
        |  },
        |  {
        |    "class": "freechips.rocketchip.util.SRAMAnnotation",
        |    "target": "TestHarness.Directory.cc_dir",
        |    "address_width": 10,
        |    "name": "cc_dir",
        |    "data_width": 136,
        |    "depth": 1024,
        |    "description": "Directory RAM",
        |    "write_mask_granularity": 17
        |  }
        |]
        |""".stripMargin
  }
}
