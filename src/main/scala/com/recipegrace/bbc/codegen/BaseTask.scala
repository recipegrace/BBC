package com.recipegrace.bbc.codegen

import com.recipegrace.bbc.grmr.Expressions.Expression

/**
  * Created by Ferosh Jacob on 2/4/17.
  */
trait BaseTask extends ExpressionCreator {



  def createBaseServiceTask(id: Int, name: String, className: String, properties: List[(String, Expression)]):Any


}
