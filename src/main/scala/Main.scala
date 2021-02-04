object Expression extends Enumeration {
  type Expression = Value
  val INT_CONST, BIN_OP, COMP, COND, LET, VAR, FN_CALL = Value
}

object Operation extends Enumeration {
  type Operation = Value
  val PLUS, MINUS, TIMES, DIV = Value
}

object ComparisonTypes extends Enumeration {
  type ComparisonTypes = Value
  val EQ = Value
}

import Expression._
import Operation._
import ComparisonTypes._

object Main {
  def eval(c: Array[_], envir: Any, functions: Any): Values = {
    val e: Expression = c(0).asInstanceOf[Expression];
    e match {
      case INT_CONST => {
        val v: Int = c(1).asInstanceOf[Int];
        return new IntValue(v);
      }
      case BIN_OP => {
        val op: Operation = c(1).asInstanceOf[Operation];
        val left: IntValue =
          eval(c(2).asInstanceOf[Array[_]], envir, functions).asInstanceOf[IntValue];
        val right: IntValue =
          eval(c(3).asInstanceOf[Array[_]], envir, functions).asInstanceOf[IntValue];
        op match {
          case PLUS => {
            return new IntValue(left.v + right.v)
          }
          case MINUS => {
            return new IntValue(left.v - right.v)
          }
          case TIMES => {
            return new IntValue(left.v * right.v)
          }
          case DIV => {
            return new IntValue(left.v / right.v)
          }
        }
      }
      case COMP => {
        val compType: ComparisonTypes = c(1).asInstanceOf[ComparisonTypes];
        val left: Values =
          eval(c(2).asInstanceOf[Array[_]], envir, functions).asInstanceOf[Values];
        val right: Values =
          eval(c(3).asInstanceOf[Array[_]], envir, functions).asInstanceOf[Values];
        compType match {
          case EQ => {
            val result: Boolean = (((left.asInstanceOf[IntValue]).v) == ((right
              .asInstanceOf[IntValue])
              .v));
            return new BooleanValue(result)
          }
        }
      }
      case COND => {
        val cond: BooleanValue =
          eval(c(1).asInstanceOf[Array[_]], envir, functions).asInstanceOf[BooleanValue];
        if (cond.b)
          return eval(c(2).asInstanceOf[Array[_]], envir, functions)
        else
          return eval(c(3).asInstanceOf[Array[_]], envir, functions)
      }
      case LET => {
        val v: Values = eval(c(2).asInstanceOf[Array[_]], envir, functions)
        val newE: Environment = envir
          .asInstanceOf[Environment]
          .bind(c(1).asInstanceOf[Name], v.asInstanceOf[Values])
        return eval(c(3).asInstanceOf[Array[_]], newE, functions)
      }
      case VAR => {
        return envir
          .asInstanceOf[Environment]
          .lookup(
            c(1).asInstanceOf[Name],
            envir.asInstanceOf[Environment].bindings
          )
      }
      case FN_CALL => {
        val f: MyFunction = findFunction(c(1).asInstanceOf[Name], functions.asInstanceOf[Array[MyFunction]]);

        var evaluationEnvironment: Environment = envir.asInstanceOf[Environment];
        val max = f.formalNamesOfArguments.length;
        val actualArguments: Array[Array[_]]= c(2).asInstanceOf[Array[Array[_]]];

        for(i <- 0 until max){
          val actualArgumentValue: Values = eval(actualArguments(i), envir, functions);
          evaluationEnvironment = evaluationEnvironment.bind(f.formalNamesOfArguments(i), actualArgumentValue);
        }
        return eval(f.body, evaluationEnvironment, functions);
      }
    }
  }

  def findFunction(name: Name, search: Array[MyFunction]): MyFunction = {
    for(f <- search if (f != null))
      if(f.name.equals(name))
        return f
    return null;
  }
  // 474
  var p1 = Array(Expression.INT_CONST, 474)

  // 400 + (70 + 4)
  var p2 = Array(
    Expression.BIN_OP,
    Operation.PLUS,
    Array(Expression.INT_CONST, 400),
    Array(
      Expression.BIN_OP,
      Operation.PLUS,
      Array(Expression.INT_CONST, 70),
      Array(Expression.INT_CONST, 4)
    )
  )

  // 470 == (400 + 74)
  var p3 = Array(
    Expression.COMP,
    ComparisonTypes.EQ,
    Array(Expression.INT_CONST, 474),
    Array(
      Expression.BIN_OP,
      Operation.PLUS,
      Array(Expression.INT_CONST, 400),
      Array(Expression.INT_CONST, 74)
    )
  )

  // if( 10 == (5+45)/5))
  // then 474
  // else 474/0 => / by zero error
  var p4 = Array(
    Expression.COND,
    Array(
      Expression.COMP,
      ComparisonTypes.EQ,
      Array(Expression.INT_CONST, 10),
      Array(
        Expression.BIN_OP,
        Operation.DIV,
        Array(
          Expression.BIN_OP,
          Operation.PLUS,
          Array(Expression.INT_CONST, 5),
          Array(Expression.INT_CONST, 45)
        ),
        Array(Expression.INT_CONST, 5)
      )
    ),
    Array(Expression.INT_CONST, 474),
    Array(
      Expression.BIN_OP,
      Operation.DIV,
      Array(Expression.INT_CONST, 474),
      Array(Expression.INT_CONST, 0)
    )
  )

  // let bot = (5+45)/5
  //  bot
  var p5 = Array(
    Expression.LET,
    new Name("bot"),
    Array(
      Expression.BIN_OP,
      Operation.DIV,
      Array(
        Expression.BIN_OP,
        Operation.PLUS,
        Array(Expression.INT_CONST, 5),
        Array(Expression.INT_CONST, 45)
      ),
      Array(Expression.INT_CONST, 5)
    ),
    Array(Expression.VAR, new Name("operand"))
  )

//let bot = 3 in
// (let bot = 2 in bot)
// +
// (if (bot == 0) then 474/0 else (400+74)/bot)
  var p6 = Array(
    Expression.LET,
    new Name("bot"),
    Array(Expression.INT_CONST, 3),
    Array(
      Expression.LET,
      new Name("bot"),
      Array(Expression.INT_CONST, 2),
      Array(
        Expression.COND,
        Array(
          Expression.COMP,
          ComparisonTypes.EQ,
          Array(Expression.VAR, new Name("bot")),
          Array(Expression.INT_CONST, 0)
        ),
        Array(
          Expression.BIN_OP,
          Operation.DIV,
          Array(Expression.INT_CONST, 474),
          Array(Expression.INT_CONST, 0)
        ),
        Array(
          Expression.BIN_OP,
          Operation.DIV,
          Array(
            Expression.BIN_OP,
            Operation.PLUS,
            Array(Expression.INT_CONST, 400),
            Array(Expression.INT_CONST, 74)
          ),
          Array(Expression.VAR, new Name("bot"))
        )
      )
    )
  )

  val safeDivision: MyFunction = new MyFunction(
    new Name("safeDivision"),
    Array(
      Expression.COND,
      Array(
        Expression.COMP,
        ComparisonTypes.EQ,
        Array(Expression.VAR, new Name("bot")),
        Array(Expression.INT_CONST, 0)
      ),
      Array(Expression.INT_CONST, 0),
      Array(
        Expression.BIN_OP,
        Operation.DIV,
        Array(Expression.VAR, new Name("top")),
        Array(Expression.VAR, new Name("bot"))
      )
    ),
    new Name("top"),
    new Name("bot")
  )

  //safeDivision(500, 5)
  var p7 = Array(
    Expression.FN_CALL,
    new Name("safeDivision"),
    Array(
      Array(Expression.INT_CONST, 500),
      Array(Expression.INT_CONST, 5)

    )
  )
  //safeDivision(500, 0)
  var p8 = Array(
    Expression.FN_CALL,
    new Name("safeDivision"),
    Array(
      Array(Expression.INT_CONST, 500),
      Array(Expression.INT_CONST, 0)

    )
  )

  def main(args: Array[String]): Unit = {
    println("let bot = 3 in\n let bot = 2 in \n  (if (bot == 0) then 474/0 else (400+74)/bot) \nreturns: "
      + eval(p6, new Environment(), Array(safeDivision)))
    println("\n500/5 = " + eval(p7, new Environment(), Array(safeDivision)))
    println("500/0 = " + eval(p8, new Environment(), Array(safeDivision)))

  }
}

class Values {}

class IntValue(var v: Int) extends Values {
  override def toString(): String = {
    return "" + v;
  }
}

class BooleanValue(var b: Boolean) extends Values {
  override def toString(): String = {
    return "" + b;
  }
}

class FunctionValue(var theFunction: java.util.function.Function[Array[Values], Values]) extends Values{

}

class Name(var theName: String) {
  override def toString(): String = {
    return theName;
  }

  def equals(o: Name): Boolean = {
    if (this == o)
      return true
    if (o == null || this.getClass != o.getClass)
      return false
    val name: Name = o.asInstanceOf[Name]
    return theName.equals(name.theName)
  }
}

class Binding(var name: Name, var value: Values) {
  override def toString(): String = {
    return "name=" + name + " val=" + value;
  }
}

class Environment() {
  var bindings: List[Binding] = List();

  def lookup(name: Name, search: List[Binding]): Values = {
    for (b <- search if (b != null)) {
      if (b.name.equals(name))
        return b.value;
    };
    return null
  }

  def bind(name: Name, value: Values): Environment = {
    val b: Binding = new Binding(name, value)
    bindings = b :: bindings
    return this;
  }

  override def toString(): String = {
    return "Environment{ " + bindings + " }";
  }
}

class MyFunction(var name: Name, var body: Array[_], var formalNamesOfArguments: Name*) {

}