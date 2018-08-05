package com.recipegrace.bbc.codegen

import com.recipegrace.bbc.BaseTest
import com.recipegrace.bbc.grmr.Expressions.{Expression, StringExpression, VariableExpression}

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/8/16.
  */
class BaseCodeGenTest extends BaseTest with ExpressionCreator {


  def fieldValueTest(current:Map[String,Any], expected:Map[String,String]) = {
    val argList= current("config").asInstanceOf[Map[String,Any]]("run").asInstanceOf[Map[String,Any]]("args").asInstanceOf[List[String]]
    argList.head shouldBe "-cp"
    val actual= argList.drop(1).grouped(2).map(f=> f.head->f(1)).toMap
    expected.foreach(f=> {
      actual(f._1) shouldBe f._2
    })
  }
  def fieldValueTest(xmlContent: NodeSeq, nameAndValues: Map[String, String]) = {

    nameAndValues.foreach(g => {
      val filtered = (xmlContent \\ "field").filter(f => {

        f.attributes.exists(k => {
          k.get("name").nonEmpty && k.get("name").get.text == g._1
        })
      })
      assert(filtered.nonEmpty, "field doesn't exist:" + g._1)
      (filtered.head \ "@stringValue").text shouldBe g._2
    })
  }

  def fieldExpressionValueTest(xmlContent: NodeSeq, nameAndValues: Map[String, Expression]) = {

    nameAndValues.foreach(g => {
      val filtered = (xmlContent \\ "field").filter(f => {

        f.attributes.exists(k => {
          k.get("name").nonEmpty && k.get("name").get.text == g._1
        })
      })
      assert(filtered.nonEmpty, "field doesn't exist:" + g._1)

      g._2 match {
        case StringExpression(x) =>   (filtered.head \ "@stringValue").text shouldBe x
        case VariableExpression(x) =>  (filtered.head \ "expression").text shouldBe s"$${$x}"
      }

    })
  }

}
