package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait PySparkJobValidation extends BaseValidations with GrammarKeywords {

  def validatePySparkJob(pySparkJob: PySparkJob) = {

    validatePySparkJobConfigs(pySparkJob.pySparkJobConfigs, pySparkJob.name)
  }

  def validatePySparkJobConfigs(pySparkJobConfigs: List[PySparkJobConfig], name: String) = {
    mandatory(List(MAINPYFILE), pySparkJobConfigs, s"PySparkJob $name")
    configValidation(pySparkJobConfigs, s"on PySparkJob $name")
    pySparkJobConfigs.foreach {
      case PySparkJobConfigMainPyFile(StringExpression(x)) => notEmpty(x, s"$name.mainPyFile")

      case PySparkJobConfigProps(StringExpression(x)) => notEmpty(x, s"$name.pySparkJobProperties")
      case _ =>
    }
  }
}
