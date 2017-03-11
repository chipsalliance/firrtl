// See LICENSE for license details.

package logger

import java.io.{File, FileOutputStream, PrintStream}

import firrtl.ExecutionOptionsManager

import scala.util.DynamicVariable

/**
  * This provides a facility for a log4scala* type logging system.  Why did we write our own?  Because
  * the canned ones are just to darned hard to turn on, particularly when embedded in a distribution.
  * This one can be turned on programmatically or with the options exposed in the [[firrtl.CommonOptions]]
  * and [[ExecutionOptionsManager]] API's in firrtl.
  * There are 4 main options.
  *  * a simple global option to turn on all in scope (and across threads, might want to fix this)
  *  * turn on specific levels for specific fully qualified class names
  *  * set a file to write things to, default is just to use stdout
  *  * include the class names and level in the output.  This is useful to figure out what
  *  the class names that extend LazyLogger are
  *
  *  This is not overly optimized but does pass the string as () => String to avoid string interpolation
  *  occurring if the the logging level is not sufficiently high. This could be further optimized by playing
  *  with methods
  */
/**
  * The supported log levels, what do they mean? Whatever you want them to.
  */
object LogLevel extends Enumeration {
  val Error, Warn, Info, Debug, Trace = Value
}

/**
  * extend this trait to enable logging in a class you are implementing
  */
trait LazyLogging {
  val logger = new Logger(this.getClass.getName)
}

class LoggerState {
  var globalLevel = LogLevel.Error
  val classLevels = new scala.collection.mutable.HashMap[String, LogLevel.Value]
  val classToLevelCache = new scala.collection.mutable.HashMap[String, LogLevel.Value]
  var logClassNames = false
  var stream: PrintStream = System.out
}

/**
  * Singleton in control of what is supposed to get logged, how it's to be logged and where it is to be logged
  * We uses a dynamic variable in case multiple threads are used as can be in scalatests
  */
object Logger {
  private val updatableLoggerState = new DynamicVariable[Option[LoggerState]](None)
  private val state = updatableLoggerState.value.getOrElse(new LoggerState)

  def testPackageNameMatch(className: String, level: LogLevel.Value): Boolean = {
    if(state.classLevels.isEmpty) return false

    // If this class name in cache just use that value
    val levelForThisClassName = state.classToLevelCache.getOrElse(className, {
      // otherwise break up the class name in to full package path as list and find most specific entry you can
      val packageNameList = className.split("""\.""").toList
      /*
       * start with full class path, lopping off from the tail until nothing left
       */
      def matchPathToFindLevel(packageList: List[String]): LogLevel.Value = {
        if(packageList.isEmpty) {
          LogLevel.Error
        }
        else {
          val partialName = packageList.mkString(".")
          state.classLevels.getOrElse(partialName, {
            matchPathToFindLevel(packageList.reverse.tail.reverse)
          })
        }
      }
      state.classToLevelCache(className) = matchPathToFindLevel(packageNameList)
      state.classToLevelCache(className)
    })
    levelForThisClassName >= level
  }

  //scalastyle:off regex
  def showMessage(level: LogLevel.Value, className: String, message: => String): Unit = {
    if(state.globalLevel >= level || testPackageNameMatch(className, level)) {
      if(state.logClassNames) {
        state.stream.println(s"[$level:$className] $message")
      }
      else {
        state.stream.println(message)
      }
    }
  }

  def reset(): Unit = {
    state.classLevels.clear()
    clearCache()
    state.logClassNames = false
    state.globalLevel = LogLevel.Error
    state.stream = System.out
  }

  def clearCache(): Unit = {
    state.classToLevelCache.clear()
  }

  def setLevel(level: LogLevel.Value): Unit = {
    state.globalLevel = level
  }

  def setOutput(fileName: String): Unit = {
    state.stream = new PrintStream(new FileOutputStream(new File(fileName)))
  }
  def setOutput(stream: PrintStream): Unit = {
    state.stream = stream
  }
  def setConsole(): Unit = {
    state.stream = Console.out
  }
  def addClassLogLevels(namesToLevel: Map[String, LogLevel.Value]): Unit = {
    state.classLevels ++= namesToLevel
  }
  def setClassLogLevels(namesToLevel: Map[String, LogLevel.Value]): Unit = {
    state.classLevels.clear()
    clearCache()
    state.classLevels ++= namesToLevel
  }

  def setOptions(optionsManager: ExecutionOptionsManager): Unit = {
    val commonOptions = optionsManager.commonOptions
    state.globalLevel = commonOptions.globalLogLevel
    setClassLogLevels(commonOptions.classLogLevels)
    if(commonOptions.logToFile) {
      setOutput(commonOptions.getLogFileName(optionsManager))
    }
    state.logClassNames = commonOptions.logClassNames
  }
}

/**
  * Classes implementing [[LazyLogging]] will have logger of this type
  * @param containerClass  passed in from the LazyLogging trait in order to provide class level logging granularity
  */
class Logger(containerClass: String) {
  def error(message: => String): Unit = {
    Logger.showMessage(LogLevel.Error, containerClass, message)
    Logger.showMessage(LogLevel.Debug, containerClass, message)
  }
  def warn(message: => String): Unit = {
    Logger.showMessage(LogLevel.Warn, containerClass, message)
    Logger.showMessage(LogLevel.Debug, containerClass, message)
  }
  def info(message: => String): Unit = {
    Logger.showMessage(LogLevel.Info, containerClass, message)
    Logger.showMessage(LogLevel.Debug, containerClass, message)
  }
  def debug(message: => String): Unit = {
    Logger.showMessage(LogLevel.Debug, containerClass, message)
  }
  def trace(message: => String): Unit = {
    Logger.showMessage(LogLevel.Trace, containerClass, message)
    Logger.showMessage(LogLevel.Debug, containerClass, message)
  }
}
