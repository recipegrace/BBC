package com.recipegrace.bbc.grmr

/**
  * Created by Ferosh Jacob on 12/29/16.
  */
object Expressions  extends GrammarKeywords{
  sealed trait Expression{
    def value :Any
    def + (expression: Expression):Expression = {
      assert(false,s"$expression not supported for $this")
      InvalidExpression
    }
    def == (expression: Expression):Expression = {
      assert(false,s"$expression not supported for $this")
      InvalidExpression
    }
    def invalid(variableName:String,message:String) ={
      assert(false,s"$variableName:$message")
      InvalidExpression
    }
  }
  /*
    Any because it is can be identified or an operator and expression
   */
  case class ProcessVariableOperation(operation:String,expression: Any)
  case class ProcessVariableExpression(operations:List[ProcessVariableOperation]) extends Expression {
    override def value: Any = {

      if (operations.exists{
        case  ProcessVariableOperation(_, StringExpression) => true
        case  _ => false
      })
        toValue(operations, true)
       else "${" + toValue(operations, false) + "}"
    }
    def toValue(exprs:List[Any],isString:Boolean):String= exprs
     match {

        case ProcessVariableOperation("",y) :: List()  if y.isInstanceOf[String]=>  toPSVar(y.toString,isString)
        case ProcessVariableOperation("",y) :: List()  => y.asInstanceOf[Expression].value.toString
        case ProcessVariableOperation("+",ProcessVariableExpression(y)) :: List()  => toValue(y, isString)
        case ProcessVariableOperation("+",y) :: List()  => y.asInstanceOf[Expression].value.toString
        case ProcessVariableOperation("+",ProcessVariableExpression(y)) :: z  => toValue(y, isString) + (if (isString) "" else "+") + toValue(z, isString)
        case ProcessVariableOperation("+",y) :: z  =>  y.asInstanceOf[Expression].value + (if (isString) "" else "+") + toValue(z, isString)
        case ProcessVariableOperation("",y) :: z  =>  toPSVar(y.toString,isString)+ (if (isString) "" else "+") + toValue(z, isString)
       /*
        case (x:String, List()) =>  toPSVar(x, isString)
        case (x:ProcessVariableExpression, List()) =>x.toValue(x.operations,isString)
        case (x:(String,Expression), List()) if x._1==PLUS =>x._2.value.toString
        case (x:(String,ProcessVariableExpression), y:List[Any]) if x._1==PLUS => {
          x._2.toValue(List(x._2), isString) + (if (isString) "" else "+") + toValue(y, isString)
        }
        case (x:(String,Expression), y:List[Any])  if x._1==PLUS =>   x._2.value.toString + (if(isString)""else "+") +toValue(y,isString)
        case (x:String, y:List[Any]) =>   toPSVar(x,isString)+ (if(isString)""else "+")  +toValue(y,isString)
        */
      }



    private def toPSVar(x: String, isString:Boolean) = {
      if(isString)
      "$" + s"{$x}"
      else x
    }

    override def +(expression: Expression):Expression = expression match {
      case x:VariableExpression =>    this.+(x.value.asInstanceOf[Expression])
      case _ =>  ProcessVariableExpression (operations :+ ProcessVariableOperation(PLUS,expression))
    }
    override def ==(expression: Expression):Expression = expression match {
      case x:VariableExpression =>    this.+(x.value.asInstanceOf[Expression])
      case _ =>  ProcessVariableExpression (operations :+ ProcessVariableOperation(DOUBLEQUAL,expression))
    }
  }

  case object InvalidExpression extends Expression {
    override def value: Any = null
  }


  case class StringExpression(text: String) extends Expression {
    override def value: Any = text

    override def toString: String = text
    override def +(expression: Expression): Expression = {
      expression match {
        case x: StringExpression => StringExpression(text + x.text)
        case x: ProcessVariableExpression => {
          ProcessVariableExpression (ProcessVariableOperation(PLUS,this)::x.operations)
        }
        case x: VariableExpression => this.+(x.value.asInstanceOf[Expression])
        case _ => invalid(expression.value.toString, "only texts or variables of type texts can be concatenated")
      }
    }
  }

  case class VariableExpression(name: String) extends Expression {
    override def value: Any =  {

      assert(BBCStructures.variableStore.contains(name), s"variable $name not defined")
      BBCStructures.variableStore(name) match {
        case StringExpression(x)=> x
        case ConcatStringExpression(x) => x.map(f=> f.value).mkString
      }
    }

    override def +(expression: Expression): Expression = {
     value.asInstanceOf[Expression].+(expression)
    }
  }

  case class ConcatStringExpression(expressions:List[Expression]) extends Expression {
    override def value: Any = {
      expressions.map(f=>f.value).mkString
    }
  }
  case class NumberExpression(number: Int) extends Expression {
    override def value: Any = number
    override def +(expression: Expression): Expression = {
      expression match {
        case x: NumberExpression => NumberExpression(number + x.number)
        case x: ProcessVariableExpression => {
          ProcessVariableExpression (ProcessVariableOperation(PLUS,this)::x.operations)
        }
        case x: VariableExpression => this.+(x.value.asInstanceOf[Expression])
        case _ => invalid(expression.value.toString, "only numbers can be added")
      }
    }
  }

  case class BooleanExpression(boolean: Boolean) extends Expression {
    override def value: Any = boolean

  }

  case class FloatExpression(float: Float) extends Expression {
    override def value: Any = float
    override def +(expression: Expression): Expression = {
      expression match {
        case x: FloatExpression => FloatExpression(float + x.float)
        case x: ProcessVariableExpression => {
          ProcessVariableExpression ( ProcessVariableOperation(PLUS,this) ::x.operations)
        }
        case x: VariableExpression => this.+(x.value.asInstanceOf[Expression])
        case _ => invalid(expression.value.toString, "only float can be added to float")
      }
    }
  }



}