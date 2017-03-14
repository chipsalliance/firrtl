// See LICENSE for license details.

package loggertests

import java.io.{ByteArrayOutputStream, PrintStream}

import logger.{LazyLogging, LogLevel}
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}

object LoggerSpec {
  val ErrorMsg = "message error"
  val WarnMsg = "message warn"
  val InfoMsg = "message info"
  val DebugMsg = "message debug"
}

class Logger1 extends LazyLogging {
  def run(): Unit = {
    logger.error(LoggerSpec.ErrorMsg)
    logger.warn(LoggerSpec.WarnMsg)
    logger.info(LoggerSpec.InfoMsg)
    logger.debug(LoggerSpec.DebugMsg)
  }
}

class LogsInfo2 extends LazyLogging {
  def run(): Unit = {
    logger.info("logger2")
  }
}
class LogsInfo3 extends LazyLogging {
  def run(): Unit = {
    logger.info("logger3")
  }
}
class LoggerSpec extends FreeSpec with Matchers with OneInstancePerTest {
  "Logger is a simple but powerful logging system" - {
    "Following tests show how global level can control logging" - {
      "only error shows up by default" in {
        logger.Logger.invoke() {
          val captor = new OutputCaptor
          logger.Logger.setOutput(captor.printStream)

          val r1 = new Logger1
          r1.run()
          val messagesLogged = captor.getOutputStrings

          messagesLogged.contains(LoggerSpec.ErrorMsg) should be(true)
          messagesLogged.contains(LoggerSpec.WarnMsg) should be(false)
          messagesLogged.contains(LoggerSpec.InfoMsg) should be(false)
          messagesLogged.contains(LoggerSpec.DebugMsg) should be(false)
        }
      }

      "setting level to warn will result in error and warn messages" in {
        logger.Logger.invoke() {
          val captor = new OutputCaptor
          logger.Logger.setOutput(captor.printStream)
          logger.Logger.setLevel(LogLevel.Warn)

          val r1 = new Logger1
          r1.run()
          val messagesLogged = captor.getOutputStrings

          messagesLogged.contains(LoggerSpec.ErrorMsg) should be(true)
          messagesLogged.contains(LoggerSpec.WarnMsg) should be(true)
          messagesLogged.contains(LoggerSpec.InfoMsg) should be(false)
          messagesLogged.contains(LoggerSpec.DebugMsg) should be(false)
        }
      }
      "setting level to info will result in error, info, and warn messages" in {
        logger.Logger.invoke() {
          val captor = new OutputCaptor
          logger.Logger.setOutput(captor.printStream)
          logger.Logger.setLevel(logger.LogLevel.Info)

          val r1 = new Logger1
          r1.run()
          val messagesLogged = captor.getOutputStrings

          messagesLogged.contains(LoggerSpec.ErrorMsg) should be(true)
          messagesLogged.contains(LoggerSpec.WarnMsg) should be(true)
          messagesLogged.contains(LoggerSpec.InfoMsg) should be(true)
          messagesLogged.contains(LoggerSpec.DebugMsg) should be(false)
        }
      }
      "setting level to debug will result in error, info, debug, and warn messages" in {
        logger.Logger.invoke() {
          val captor = new OutputCaptor
          logger.Logger.setOutput(captor.printStream)

          logger.Logger.setLevel(LogLevel.Error)
          logger.Logger.setOutput(captor.printStream)
          logger.Logger.setLevel(logger.LogLevel.Debug)

          val r1 = new Logger1
          r1.run()
          val messagesLogged = captor.getOutputStrings

          messagesLogged.contains(LoggerSpec.ErrorMsg) should be(true)
          messagesLogged.contains(LoggerSpec.WarnMsg) should be(true)
          messagesLogged.contains(LoggerSpec.InfoMsg) should be(true)
          messagesLogged.contains(LoggerSpec.DebugMsg) should be(true)
        }
      }
    }
    "the following tests show how logging can be controlled by package and class name" - {
      "only capture output by class name" - {
        "capture logging from LogsInfo2" in {
          logger.Logger.invoke() {
            val captor = new OutputCaptor
            logger.Logger.setOutput(captor.printStream)

            logger.Logger.setLevel("loggertests.LogsInfo2", logger.LogLevel.Info)

            val r2 = new LogsInfo2
            val r3 = new LogsInfo3
            r3.run()
            r2.run()

            val messagesLogged = captor.getOutputStrings

            messagesLogged.contains("logger3") should be(false)
            messagesLogged.contains("logger2") should be(true)
          }
        }
        "capture logging from LogsInfo2 using class" in {
          logger.Logger.invoke() {
            val captor = new OutputCaptor
            logger.Logger.setOutput(captor.printStream)

            logger.Logger.setLevel(classOf[LogsInfo2], logger.LogLevel.Info)

            val r2 = new LogsInfo2
            val r3 = new LogsInfo3
            r3.run()
            r2.run()

            val messagesLogged = captor.getOutputStrings

            messagesLogged.contains("logger3") should be(false)
            messagesLogged.contains("logger2") should be(true)
          }
        }
        "capture logging from LogsInfo3" in {
          logger.Logger.invoke() {
            val captor = new OutputCaptor
            logger.Logger.setOutput(captor.printStream)

            logger.Logger.setLevel("loggertests.LogsInfo3", logger.LogLevel.Info)

            val r2 = new LogsInfo2
            val r3 = new LogsInfo3
            r2.run()
            r3.run()

            val messagesLogged = captor.getOutputStrings

            messagesLogged.contains("logger2") should be(false)
            messagesLogged.contains("logger3") should be(true)
          }
        }
      }
      "log based on package name" - {
        "both log because of package, also showing re-run after change works" in {
          logger.Logger.invoke() {
            val captor = new OutputCaptor
            logger.Logger.setOutput(captor.printStream)

            logger.Logger.setLevel(LogLevel.Error)
            logger.Logger.setLevel("loggertests", logger.LogLevel.Error)

            val r2 = new LogsInfo2
            val r3 = new LogsInfo3
            r2.run()
            r3.run()

            var messagesLogged = captor.getOutputStrings

            messagesLogged.contains("logger2") should be(false)
            messagesLogged.contains("logger3") should be(false)

            logger.Logger.setLevel("loggertests", logger.LogLevel.Debug)

            r2.run()
            r3.run()

            messagesLogged = captor.getOutputStrings

            messagesLogged.contains("logger2") should be(true)
            messagesLogged.contains("logger3") should be(true)
          }
        }
      }
      "check for false positives" in {
        logger.Logger.invoke() {
          val captor = new OutputCaptor
          logger.Logger.setOutput(captor.printStream)

          logger.Logger.setLevel("bad-loggertests", logger.LogLevel.Info)

          val r2 = new LogsInfo2
          val r3 = new LogsInfo3
          r2.run()
          r3.run()

          val messagesLogged = captor.getOutputStrings

          messagesLogged.contains("logger2") should be(false)
          messagesLogged.contains("logger3") should be(false)
        }
      }
      "show that class specific level supercedes global level" in {
        logger.Logger.invoke() {
          val captor = new OutputCaptor
          logger.Logger.setOutput(captor.printStream)


          logger.Logger.setLevel(logger.LogLevel.Info)
          logger.Logger.setLevel("loggertests.LogsInfo2", logger.LogLevel.Error)

          val r2 = new LogsInfo2
          val r3 = new LogsInfo3
          r2.run()
          r3.run()

          val messagesLogged = captor.getOutputStrings

          messagesLogged.contains("logger2") should be(false)
          messagesLogged.contains("logger3") should be(true)
        }
      }
      "Show tests can be set with command options" in {
        logger.Logger.invoke(Array("--class-log-level", "loggertests.LogsInfo3:info")) {
          val captor = new OutputCaptor
          logger.Logger.setOutput(captor.printStream)

          val r2 = new LogsInfo2
          val r3 = new LogsInfo3
          r2.run()
          r3.run()

          val messagesLogged = captor.getOutputStrings

          messagesLogged.contains("logger2") should be(false)
          messagesLogged.contains("logger3") should be(true)
        }
      }
    }
  }
}

class OutputCaptor {
  val byteArrayOutputStream = new ByteArrayOutputStream()
  val printStream = new PrintStream(byteArrayOutputStream)
  def getOutputStrings: Seq[String] = {
    byteArrayOutputStream.toString.split("""\n""")
  }
}
