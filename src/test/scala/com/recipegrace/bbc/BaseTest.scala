package com.recipegrace.bbc

import java.util.logging.{Level, Logger}

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by Ferosh Jacob on 12/24/16.
  */
abstract class BaseTest extends FunSuite with Matchers {



  val logger = Logger.getLogger(this.getClass.getName)
  logger.setLevel(Level.ERROR)
  def println(x:Any) = {

    logger.info(x+"")
  }

}
