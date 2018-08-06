package com.recipegrace.bbc

import java.io.Reader
import java.util.logging.{Level, Logger}

import com.recipegrace.bbc.grmr.{BBC, BBCGrammar}
import com.recipegrace.bbc.grmr.BBCStructures.VariableDeclaration
import com.recipegrace.bbc.workflow.GenerateFlows

/**
  * Created by Ferosh Jacob on 11/25/16.
  */
object ActivitiMain extends BBCGrammar {
  val logger = Logger.getLogger(this.getClass.getName)

  var variables:List[VariableDeclaration]=List()
  var errorMessage = ""

  def generateProcess(dsl: String): Option[scala.xml.Elem] = {
    val bbc = parseAll(_bbc, dsl)
    bbcToElem(bbc)


  }

  def generateProcess(reader: Reader): Option[scala.xml.Elem] = {


    val bbc = parseAll(_bbc, reader)
    bbcToElem(bbc)
  }

  def bbcToElem(bbc: ActivitiMain.ParseResult[BBC]) = {
    try {
      bbc match {
        case Success(result, _) => {

          val xml = GenerateFlows(result).generateActiviti
          logger.setLevel(Level.OFF)
          logger.info("parse success!")
          variables= result.variables
          errorMessage=""
          Some(xml)
        }
        case Failure(msg, _) => {
          errorMessage = msg
          logger.setLevel(Level.SEVERE)
          logger.info("parse error:" + errorMessage)
          None
        }
        case Error(msg, _) => {
          logger.setLevel(Level.SEVERE)
          logger.info("ERROR:" + msg)
          None
        }
      }
    } catch {
      case x: Throwable => {
        logger.setLevel(Level.SEVERE)
        logger.info("parse error:" + x.getMessage)
        errorMessage = x.getMessage
        None
      }
    }
  }
}
