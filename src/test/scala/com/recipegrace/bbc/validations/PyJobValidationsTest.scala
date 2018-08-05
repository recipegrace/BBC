package com.recipegrace.bbc.validations


/**
  * Created by Ferosh Jacob on 12/26/16.
  */
class PyJobValidationsTest extends BaseRepositoryJobTest {
  override def jobName: String = "pythonjob"

  override def validLine: String = """mainPyFile="hello" """

  override def errorLine1: String = """mainPyFile=temp """

  override def emptyLine: String = """mainPyFile="" """

  override def fieldName: String = "mainPyFile"
}
