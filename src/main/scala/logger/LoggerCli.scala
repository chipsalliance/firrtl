// See LICENSE for license details.

package logger

import firrtl.options.Shell

/** Include Logger command line options in an options manager */
trait LoggerCli { this: Shell =>
  parser.note("Logger Options")

  Seq( LogLevelAnnotation,
       ClassLogLevelAnnotation,
       LogFileAnnotation,
       LogClassNamesAnnotation )
    .map(_.addOptions(parser))
}
