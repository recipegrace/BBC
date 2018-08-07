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
        case x:JSONObject =>x.toString()
        case x:JSONArray => x.toString()
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
  /**
    * Created by Ferosh Jacob on 12/29/16.
    */
  sealed abstract class JSONType extends Expression {
    /**
      * This version of toString allows you to provide your own value
      * formatter.
      */
    def toString (formatter : JSONFormat.ValueFormatter) : String

    /**
      * Returns a String representation of this JSON value
      * using the JSONFormat.defaultFormatter.
      */
    override def toString = toString(JSONFormat.defaultFormatter)

    override def value: Any = toString(JSONFormat.defaultFormatter)
  }

  /**
    * This object defines functions that are used when converting JSONType
    * values into String representations. Mostly this is concerned with
    * proper quoting of strings.
    *
    * @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
    */
  object JSONFormat {
    /**
      * This type defines a function that can be used to
      * format values into JSON format.
      */
    type ValueFormatter = Any => String

    /**
      * The default formatter used by the library. You can
      * provide your own with the toString calls on
      * JSONObject and JSONArray instances.
      */
    val defaultFormatter : ValueFormatter = (x : Any) => x match {
      case s : String =>  s
      case jo : JSONObject => jo.toString(defaultFormatter)
      case ja : JSONArray => ja.toString(defaultFormatter)
      case other => other.toString
    }

    /**
      * This function can be used to properly quote Strings
      * for JSON output.
      */
    def quoteString (s : String) : String =
      s.map {
        case '"'  => "\\\""
        case '\\' => "\\\\"
        case '/'  => "\\/"
        case '\b' => "\\b"
        case '\f' => "\\f"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        /* We'll unicode escape any control characters. These include:
         * 0x0 -> 0x1f  : ASCII Control (C0 Control Codes)
         * 0x7f         : ASCII DELETE
         * 0x80 -> 0x9f : C1 Control Codes
         *
         * Per RFC4627, section 2.5, we're not technically required to
         * encode the C1 codes, but we do to be safe.
         */
        case c if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) => "\\u%04x".format(c.toInt)
        case c => c
      }.mkString
  }

  /**
    *  Represents a JSON Object (map).
    *
    *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
    */
  case class JSONObject (obj : Map[String,Any]) extends JSONType {
    def toString (formatter : JSONFormat.ValueFormatter) =
      "{" + obj.map({ case (k,v) => formatter(k.toString) + " : " + formatter(v) }).mkString(", ") + "}"


  }

  /**
    *  Represents a JSON Array (list).
    *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
    */
  case class JSONArray (list : List[Any]) extends JSONType {
    def toString (formatter : JSONFormat.ValueFormatter) =
      "[" + list.map(formatter).mkString(", ") + "]"

  }


}