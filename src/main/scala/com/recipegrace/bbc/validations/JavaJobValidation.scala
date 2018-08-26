package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait JavaJobValidation extends BaseValidations with GrammarKeywords {

  def validateJavaJob(javaJob: JavaJob) = {

    validateJavaJobConfigs(javaJob.javaJobConfigs, javaJob.name)
  }


  def validateJavaJobConfigs(jobConfigs: List[JavaJobConfig], name: String) = {
    mandatory(List(JARLOCATION,MAINCLASS), jobConfigs, s"JavaJob $name")
    configValidation(jobConfigs, s"on JavaJob $name")
    jobConfigs.foreach {
      case MainClassJavaJobConfig(StringExpression(x)) => notEmpty(x, s"$name.mainClass")
      case JarLocationJavaJobConfig(StringExpression(x)) => notEmpty(x, s"$name.jarLocation")

      case _ =>
    }
  }
}
