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

      val parsingError = intercept[AnnotationClassNotFoundException] {
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
        "org.json4s.package$MappingException: Do not know how to deserialize 'freechips.rocketchip.util.RegFieldDescMappingAnnotation'"
      )
      output should include(
        "org.json4s.package$MappingException: Do not know how to deserialize 'freechips.rocketchip.util.SRAMAnnotation'"
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
    val s = new StringBuilder()

    s ++= """["""

    if (includeAllowUnrecognizedAnnotations) {
      s ++= """  {"""
      s ++= """    "class":"firrtl.stage.AllowUnrecognizedAnnotations$""""
      s ++= """  },"""
    }
    s ++= """  {"""
    s ++= """    "class":"firrtl.transforms.BlackBoxInlineAnno","""
    s ++= """    "target":"TestHarness.plusarg_reader_27","""
    s ++= """    "name":"plusarg_reader.v","""
    s ++= """    "text":"// See LICENSE.SiFive for license details.\n\n//VCS coverage exclude_file\n\n// No default parameter values are intended, nor does IEEE 1800-2012 require them (clause A.2.4 param_assignment),\n// but Incisive demands them. These default values should never be used.\nmodule plusarg_reader #(\n   parameter FORMAT=\"borked=%d\",\n   parameter WIDTH=1,\n   parameter [WIDTH-1:0] DEFAULT=0\n) (\n   output [WIDTH-1:0] out\n);\n\n`ifdef SYNTHESIS\nassign out = DEFAULT;\n`else\nreg [WIDTH-1:0] myplus;\nassign out = myplus;\n\ninitial begin\n   if (!$value$plusargs(FORMAT, myplus)) myplus = DEFAULT;\nend\n`endif\n\nendmodule\n""""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "class":"freechips.rocketchip.util.RegFieldDescMappingAnnotation","""
    s ++= """    "target":"TestHarness.PeripheryBus","""
    s ++= """    "regMappingSer":{"""
    s ++= """    "displayName":"PeripheryBus","""
    s ++= """    "deviceName":"PeripheryBus","""
    s ++= """    "baseAddress":16384,"""
    s ++= """    "regFields":["""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":0,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_0","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":8,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_8","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":16,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_16","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":24,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_24","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":32,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_32","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":40,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_40","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":48,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_48","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "byteOffset":"0x0","""
    s ++= """    "bitOffset":56,"""
    s ++= """    "bitWidth":8,"""
    s ++= """    "name":"unnamedRegField0_56","""
    s ++= """    "resetValue":0,"""
    s ++= """    "accessType":"None","""
    s ++= """    "wrType":"None","""
    s ++= """    "rdAction":"None","""
    s ++= """    "desc":"None","""
    s ++= """    "group":"None","""
    s ++= """    "groupDesc":"None","""
    s ++= """    "volatile":false,"""
    s ++= """    "hasReset":false,"""
    s ++= """    "enumerations":{"""
    s ++= """"""
    s ++= """  }"""
    s ++= """  }"""
    s ++= """    ]"""
    s ++= """  }"""
    s ++= """  },"""
    s ++= """  {"""
    s ++= """    "class":"freechips.rocketchip.util.SRAMAnnotation","""
    s ++= """    "target":"TestHarness.Directory.cc_dir","""
    s ++= """    "address_width":10,"""
    s ++= """    "name":"cc_dir","""
    s ++= """    "data_width":136,"""
    s ++= """    "depth":1024,"""
    s ++= """    "description":"Directory RAM","""
    s ++= """    "write_mask_granularity":17"""
    s ++= """  }"""
    s ++= """]"""

    s.toString
  }
}
