package com.recipegrace.bbc.codegen

import com.recipegrace.bbc.grmr.Expressions._
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 12/29/16.
  */
trait ExpressionCreator extends GrammarKeywords {


  def expr(text:String):Expression = StringExpression(text)
  def expr(text:Int):Expression = StringExpression(text+"")


  def evaluateVariable(expression: Expression,localVariables:Map[String,Expression]):Any= {
    expression match {
      case ConcatStringExpression(x) => {
        val result=x.map(f=> evaluateVariable(f,localVariables))
        result.mkString
      }
      case VariableExpression(x)  if localVariables.contains(x) => localVariables(x).value
      case x:VariableExpression=> {
        x.value
      }
      case _ =>   expression.value
    }
  }

}
