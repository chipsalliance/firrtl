package firrtl.traversals

import firrtl.HasMapWidth
import firrtl.ir._

object Foreachers {

  // ********** Stmt Mappers **********
  private trait StmtMagnet {
    def foreach(stmt: Statement): Unit
  }
  private object StmtMagnet {
    implicit def forStmt(f: Statement => Unit): StmtMagnet = new StmtMagnet {
      override def foreach(stmt: Statement): Unit = stmt foreachStmt f
    }
    implicit def forExp(f: Expression => Unit): StmtMagnet = new StmtMagnet {
      override def foreach(stmt: Statement): Unit = stmt foreachExpr f
    }
    implicit def forType(f: Type => Unit): StmtMagnet = new StmtMagnet {
      override def foreach(stmt: Statement) : Unit = stmt foreachType f
    }
    implicit def forString(f: String => Unit): StmtMagnet = new StmtMagnet {
      override def foreach(stmt: Statement): Unit = stmt foreachString f
    }
    implicit def forInfo(f: Info => Unit): StmtMagnet = new StmtMagnet {
      override def foreach(stmt: Statement): Unit = stmt foreachInfo f
    }
  }
  implicit class StmtForeach(val _stmt: Statement) extends AnyVal {
    // Using implicit types to allow overloading of function type to foreach, see StmtMagnet above
    def foreach[T](f: T => Unit)(implicit magnet: (T => Unit) => StmtMagnet): Unit = magnet(f).foreach(_stmt)
  }

  // ********** Expression Mappers **********
  private trait ExprMagnet {
    def foreach(expr: Expression): Unit
  }
  private object ExprMagnet {
    implicit def forExpr(f: Expression => Unit): ExprMagnet = new ExprMagnet {
      override def foreach(expr: Expression): Unit = expr foreachExpr f
    }
    implicit def forType(f: Type => Unit): ExprMagnet = new ExprMagnet {
      override def foreach(expr: Expression): Unit = expr foreachType f
    }
    implicit def forWidth(f: Width => Unit): ExprMagnet = new ExprMagnet {
      override def foreach(expr: Expression): Unit = expr foreachWidth f
    }
  }
  implicit class ExprMap(val _expr: Expression) extends AnyVal {
    def foreach[T](f: T => Unit)(implicit magnet: (T => Unit) => ExprMagnet): Unit = magnet(f).foreach(_expr)
  }

  // ********** Type Mappers **********
  private trait TypeMagnet {
    def foreach(tpe: Type): Unit
  }
  private object TypeMagnet {
    implicit def forType(f: Type => Unit): TypeMagnet = new TypeMagnet {
      override def foreach(tpe: Type): Unit = tpe foreachType f
    }
    implicit def forWidth(f: Width => Unit): TypeMagnet = new TypeMagnet {
      override def foreach(tpe: Type): Unit = tpe foreachWidth f
    }
  }
  implicit class TypeMap(val _tpe: Type) extends AnyVal {
    def foreach[T](f: T => Unit)(implicit magnet: (T => Unit) => TypeMagnet): Unit = magnet(f).foreach(_tpe)
  }

  // ********** Module Mappers **********
  private trait ModuleMagnet {
    def foreach(module: DefModule): Unit
  }
  private object ModuleMagnet {
    implicit def forStmt(f: Statement => Unit): ModuleMagnet = new ModuleMagnet {
      override def foreach(module: DefModule): Unit = module foreachStmt f
    }
    implicit def forPorts(f: Port => Unit): ModuleMagnet = new ModuleMagnet {
      override def foreach(module: DefModule): Unit = module foreachPort f
    }
    implicit def forString(f: String => Unit): ModuleMagnet = new ModuleMagnet {
      override def foreach(module: DefModule): Unit = module foreachString f
    }
    implicit def forInfo(f: Info => Unit): ModuleMagnet = new ModuleMagnet {
      override def foreach(module: DefModule): Unit = module foreachInfo f
    }
  }
  implicit class ModuleMap(val _module: DefModule) extends AnyVal {
    def foreach[T](f: T => Unit)(implicit magnet: (T => Unit) => ModuleMagnet): Unit = magnet(f).foreach(_module)
  }

  // ********** Circuit Mappers **********
  private trait CircuitMagnet {
    def foreach(module: Circuit): Unit
  }
  private object CircuitMagnet {
    implicit def forModules(f: DefModule => Unit): CircuitMagnet = new CircuitMagnet {
      override def foreach(circuit: Circuit): Unit = circuit foreachModule f
    }
    implicit def forString(f: String => Unit): CircuitMagnet = new CircuitMagnet {
      override def foreach(circuit: Circuit): Unit = circuit foreachString f
    }
    implicit def forInfo(f: Info => Unit): CircuitMagnet = new CircuitMagnet {
      override def foreach(circuit: Circuit): Unit = circuit foreachInfo f
    }
  }
  implicit class CircuitMap(val _circuit: Circuit) extends AnyVal {
    def foreach[T](f: T => Unit)(implicit magnet: (T => Unit) => CircuitMagnet): Unit = magnet(f).foreach(_circuit)
  }
}
