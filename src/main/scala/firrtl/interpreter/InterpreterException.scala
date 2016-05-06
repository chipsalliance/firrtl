package firrtl.interpreter

/**
  * Created by chick on 4/21/16.
  */
class InterpreterException(message: String) extends Exception(message)
object InterpreterException {
  def apply(message: String): InterpreterException = new InterpreterException(message: String)
}

