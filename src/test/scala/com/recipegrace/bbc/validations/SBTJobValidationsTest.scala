package com.recipegrace.bbc.validations

import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 12/26/16.
  */
class SBTJobValidationsTest extends BaseRepositoryJobTest {
  override def jobName: String = "sbtjob"

  override def validLine: String = """mainClass= "hello"  """

  override def errorLine1: String =  """mainClass=temp"""

  override def emptyLine: String = """mainClass= "" """

  override def fieldName: String = "mainClass"
}
