package com.recipegrace.bbc

import java.io.Reader
import java.util.logging.{Level, Logger}

import com.recipegrace.bbc.grmr.BBCStructures.VariableDeclaration
import com.recipegrace.bbc.grmr.{BBC, BBCGrammar, BBCStructures}
import com.recipegrace.bbc.concourse.YAMLGenerator
import com.recipegrace.bbc.workflow.GenerateFlows

/**
  * Created by Ferosh Jacob on 11/25/16.
  */
abstract class BaseTemplateMain extends BBCGrammar {
  val logger = Logger.getLogger(this.getClass.getName)
  var generateFlows:GenerateFlows =null

  var errorMessage = ""
  def generateProcess(dsl: String): Option[String] = {
    BBCStructures.variableStore=Map()
    val bbc = parseAll(_bbc, dsl)
    toText(bbc)


  }

  def generateProcess(reader: Reader): Option[String] = {
    BBCStructures.variableStore=Map()
    val bbc = parseAll(_bbc, reader)
    toText(bbc)
  }

  def toText(bbc: ParseResult[BBC]):Option[String] = {
    try {
      bbc match {
        case Success(result, _) => {
          logger.setLevel(Level.OFF)
          logger.info("parse success!")
          errorMessage=""
          onSuccessParse(result)
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

  def onSuccessParse(result: BBC):Option[String]
}
