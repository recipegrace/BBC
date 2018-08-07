package com.recipegrace.bbc.grmr

import com.recipegrace.bbc.grmr.BBCStructures.{VariableDeclaration, _}
import com.recipegrace.bbc.grmr.Expressions._

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.json.Parser



trait BaseGrammar extends JavaTokenParsers with GrammarKeywords {
  /*Variables*/


  def _json       = jsonObj | jsonArray
  def jsonObj    = "{" ~> repsep(objEntry, ",") <~ "}" ^^ { case vals : List[_] => JSONObject(Map(vals : _*)) }
  def jsonArray  = "[" ~> repsep(jsonValue, ",") <~ "]" ^^ { case vals : List[_] => JSONArray(vals) }
  def objEntry   = stringLiteral ~ (":" ~> jsonValue) ^^ { case x ~ y => (x, y) }
  def jsonValue: Parser[Any] = jsonObj | jsonArray | wholeNumber | "true" ^^^ true | "false" ^^^ false | "null" ^^^ null | stringLiteral



  def _fraction: Parser[String]  ="""[+-]?[0-9]*((\.[0-9]+([eE][+-]?[0-9]+)?[fF]?)|([fF])|([eE][+‌​-]?[0-9]+))\b""".r


  def _term: Parser[Expression] = (TRUE ^^ (f=>BooleanExpression(true))
    | FALSE ^^ (f=>   {
    assert(false,s"only string variables are supported")
    BooleanExpression(false)
  })
    | _fraction ^^ (f=> {
    assert(false,s"only string variables are supported")
    FloatExpression(f.toFloat)
  })
    | wholeNumber ^^ (f=> {
    assert(false,s"only string variables are supported")
    NumberExpression(f.toInt)
  })
    | ident ^^(f=> VariableExpression(f))
    |stringLiteral ^^ (f=> StringExpression(unStripQuote(f)))
    |_json ^^ (f=> f)
    )



  def _expression:Parser[Expression] =  _term ~ rep(s"[$PLUS]".r ~ _term) ^^ (f=> {

    if(f._2.isEmpty) f._1
    else {
      assert((f._1::f._2.map(f=>f._2)).forall(f=>f.isInstanceOf[StringExpression] ||f.isInstanceOf[VariableExpression]),s"+ operation defined only for string variables but found: ${
        (f._1.getClass.getSimpleName.split("\\.").last.split("\\$").last :: f._2.map(f=>f._2.getClass.getSimpleName.split("\\.").last.split("\\$").last))
      .distinct.mkString(",")}" )
      ConcatStringExpression(f._1:: f._2.map(f=>f._2))
    }
  })


  def unStripQuote(str: String): String = str.substring(1, str.length - 1)


  def _variable: Parser[List[VariableDeclaration]] = repsep(ident ~ opt(EQUAL ~ _expression) ^^
    (f=>{

      assert(!variableStore.contains(f._1), s"${f._1} already declared")
      if(f._2.isDefined)
      VariableDeclaration(f._1,   createDeclaration(f))
      else {
        assert(false,s"variable ${f._1}  should be intialized!")
        VariableDeclaration(f._1,None)
      }
    }), COMMA)

  /*(f => {
    f.map(f=> VariableDeclaration(f._1,f._2.map(g=> {



    })))
  })*/

  private def createDeclaration(f: ~[String, Option[~[String, Expression]]]) = {
    f._2.map(g => {
      g._2 match {

        case x: VariableExpression if !variableStore.contains(x.name) => {

          assert(false, s"${x.name} is not defined")
          InvalidExpression
        }
        case x: VariableExpression if variableStore.contains(x.name) => {

          variableStore = variableStore ++ Map(f._1 -> variableStore(x.name))
          variableStore(x.name)
        }
        case x: Expression => {
          variableStore = variableStore ++ Map(f._1 -> g._2)
          g._2
        }

      }
    })
  }

  def _variableDeclarations: Parser[(String,VariableDeclarations)] = VAR ~ _variable ^^ (f => {
    (VAR, VariableDeclarations(f._2))
  })




}



