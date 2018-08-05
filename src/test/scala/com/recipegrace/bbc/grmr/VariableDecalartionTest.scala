package com.recipegrace.bbc.grmr

import com.recipegrace.bbc.ActivitiMain
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions._
/**
  * Created by Ferosh Jacob on 12/27/16.
  */
class VariableDecalartionTest extends BaseBBCGrammarTest{



  test("variables test") {

   val dsl= s"""
      name= "example"
      zone = "us-east1-c"
      env = "search"
      ${ (for {i <- 1 to 10} yield "var i"+i+" =\""+i+"\"").mkString("\n") }
      delete "gs:/sdsd/"
    """.stripMargin

    val main = ActivitiMain
    main.generateProcess(dsl)

    val expected = (1 to 10).map(f=>"i"+f).toList

    main.variables.map(f=>f.name) shouldBe expected



  }

  test("process variables") {
    parseVariables( "var a=\"test\",b=a")("b").value shouldBe "test"
    parseVariables( "var a=\"hello\",b=a+\"test\",d=\"world\",c=d+b+\"some\"")("c").value shouldBe "worldhellotestsome"
  }


  test("expression string") {


    val varMap: Map[String, Expression] = parseVariables("var a=\"hello\", b=a,c=b+\"world\"+a")
    varMap("a").value shouldBe "hello"
    varMap("b").value shouldBe  "hello"
    varMap("c").value shouldBe "helloworldhello"





  }



  private def parseVariables(script: String) = {
    variableStore= Map()
    val vars = parseAll(_variableDeclarations, script).get
    val varMap = vars._2.variables.filter(f=>f.valueOption.isDefined).map(f => f.name -> f.value).toMap
    varMap
  }



  test("expression parsing") {
    val variable = "variable"
    val s1= s""""$variable""""
    val s2 = s"$variable"
    StringExpression(variable) shouldBe   parseAll(_expression,s1).get
    VariableExpression(variable) shouldBe parseAll(_expression,s2).get
  }


  private def checkVariableDeclarations(input: Map[String, Any], script: String) = {
    variableStore=Map()
    val vars = parseAll(_variableDeclarations, script).get._2.variables
    println(variableStore)
    vars should have size input.size
    val output = vars.map(f=> f.name -> f.value).toMap
    for ((x,y) <- input) {
      output(x).value.toString shouldBe y.toString.replaceAll("\"","")
    }
  }
}
