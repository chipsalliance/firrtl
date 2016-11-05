// See LICENSE for license details.

package firrtl

import firrtl.ir._

// TODO: Implement remaining mappers and recursive mappers
object Mappers {

  // ********** Stmt Mappers **********
  private trait StmtMagnet {
    def map(stmt: Statement): Statement
  }
  private object StmtMagnet {
    implicit def forStmt(f: Statement => Statement): StmtMagnet = new StmtMagnet {
      override def map(stmt: Statement): Statement = stmt mapStmt f
    }
    implicit def forExp(f: Expression => Expression): StmtMagnet = new StmtMagnet {
      override def map(stmt: Statement): Statement = stmt mapExpr f
    }
    implicit def forType(f: Type => Type): StmtMagnet = new StmtMagnet {
      override def map(stmt: Statement) : Statement = stmt mapType f
    }
    implicit def forString(f: String => String): StmtMagnet = new StmtMagnet {
      override def map(stmt: Statement): Statement = stmt mapString f
    }
  }
  implicit class StmtMap(val stmt: Statement) extends AnyVal {
    // Using implicit types to allow overloading of function type to map, see StmtMagnet above
    def map[T](f: T => T)(implicit magnet: (T => T) => StmtMagnet): Statement = magnet(f).map(stmt)
  }

  // ********** Expression Mappers **********
  private trait ExprMagnet {
    def map(expr: Expression): Expression
  }
  private object ExprMagnet {
    implicit def forExpr(f: Expression => Expression): ExprMagnet = new ExprMagnet {
      override def map(expr: Expression): Expression = expr mapExpr f
    }
    implicit def forType(f: Type => Type): ExprMagnet = new ExprMagnet {
      override def map(expr: Expression): Expression = expr mapType f
    }
    implicit def forWidth(f: Width => Width): ExprMagnet = new ExprMagnet {
      override def map(expr: Expression): Expression = expr mapWidth f
    }
  }
  implicit class ExprMap(val expr: Expression) extends AnyVal {
    def map[T](f: T => T)(implicit magnet: (T => T) => ExprMagnet): Expression = magnet(f).map(expr)
  }

  // ********** Type Mappers **********
  private trait TypeMagnet {
    def map(tpe: Type): Type
  }
  private object TypeMagnet {
    implicit def forType(f: Type => Type): TypeMagnet = new TypeMagnet {
      override def map(tpe: Type): Type = tpe mapType f
    }
    implicit def forWidth(f: Width => Width): TypeMagnet = new TypeMagnet {
      override def map(tpe: Type): Type = tpe mapWidth f
    }
  }
  implicit class TypeMap(val tpe: Type) extends AnyVal {
    def map[T](f: T => T)(implicit magnet: (T => T) => TypeMagnet): Type = magnet(f).map(tpe)
  }

  // ********** Width Mappers **********
  private trait WidthMagnet {
    def map(width: Width): Width
  }
  private object WidthMagnet {
    implicit def forWidth(f: Width => Width): WidthMagnet = new WidthMagnet {
      override def map(width: Width): Width = width match {
        case mapable: HasMapWidth => mapable mapWidth f // WIR
        case other => other // Standard IR nodes
      }
    }
  }
  implicit class WidthMap(val width: Width) extends AnyVal {
    def map[T](f: T => T)(implicit magnet: (T => T) => WidthMagnet): Width = magnet(f).map(width)
  }

  // ********** Module Mappers **********
  private trait ModuleMagnet {
    def map(module: DefModule): DefModule
  }
  private object ModuleMagnet {
    implicit def forStmt(f: Statement => Statement): ModuleMagnet = new ModuleMagnet {
      override def map(module: DefModule): DefModule = module mapStmt f
    }
    implicit def forPorts(f: Port => Port): ModuleMagnet = new ModuleMagnet {
      override def map(module: DefModule): DefModule = module mapPort f
    }
    implicit def forString(f: String => String): ModuleMagnet = new ModuleMagnet {
      override def map(module: DefModule): DefModule = module mapString f
    }
  }
  implicit class ModuleMap(val module: DefModule) extends AnyVal {
    def map[T](f: T => T)(implicit magnet: (T => T) => ModuleMagnet): DefModule = magnet(f).map(module)
  }

}
