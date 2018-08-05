package com.recipegrace.bbc.grmr

import com.recipegrace.bbc.grmr.Expressions.{ConcatStringExpression, NumberExpression, StringExpression, VariableExpression}

/**
  * Created by Ferosh Jacob on 12/29/16.
  */
class ExpressionsTest extends BaseBBCGrammarTest{


  test("expression eval test") {

    parseAll(_expression,"\"2\"+\"3\"").get.asInstanceOf[ConcatStringExpression].expressions.map(f=>f.value).mkString shouldBe "23"

  }

}
