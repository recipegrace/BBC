package com.recipegrace.bbc.validations

import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 12/30/16.
  */
class VariableValidationTest extends BaseWorkflowTest{


  test("undeclared variables") {


    assertErrorOnParser(_variableDeclarations,"delta is not defined","var alpha=\"test\", beta=alpha, ceta=delta")

  }
  test("invalid operation") {


    assertErrorOnParser(_variableDeclarations,"only string variables are supported","var a=\"s\"+10")
    assertErrorOnParser(_variableDeclarations,"only string variables are supported","var a=10+\"s\"")
    assertErrorOnParser(_variableDeclarations,"only string variables are supported","var a=10+2.0")
    assertErrorOnParser(_variableDeclarations,"only string variables are supported","var a=10.0+\"s\"")
    assertErrorOnParser(_variableDeclarations,"only string variables are supported","var a=10.0+2")
    assertErrorOnParser(_variableDeclarations,"+ operation defined only for string variables but found: JSONObject","""var x1 = {"a": "b"} + {"c": "d"}""")
    assertErrorOnParser(_variableDeclarations,"+ operation defined only for string variables but found: StringExpression,JSONObject","""var x1 = "hello" + {"c": "d"}""")
    assertErrorOnParser(_variableDeclarations,"only string variables are supported","""var x1 = 3 + {"c": "d"}""")

  }
  test("already declared") {


    assertErrorOnParser(_variableDeclarations,"a already declared","var a=\"s\",a=\"b\"")

  }

}
