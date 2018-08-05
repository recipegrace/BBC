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

  }
  test("already declared") {


    assertErrorOnParser(_variableDeclarations,"a already declared","var a=\"s\",a=\"b\"")

  }

}
