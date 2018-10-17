package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait PyJobValidation extends BaseValidations with GrammarKeywords {

  def validatePyJob(pyJob: PyJob) = {

    validatePyJobConfigs(pyJob.pyJobConfigs, pyJob.name)
  }

  def validatePyJobConfigs(pyJobConfigs: List[RepositoryJobConfig], name: String) = {
    mandatory(List(MAINPYFILE), pyJobConfigs, s"pythonjob $name")
    validateRepositoryJobConfigs(pyJobConfigs,name)
    configValidation(pyJobConfigs, s"on pythonjob $name")
    pyJobConfigs.foreach {
      case PyJobConfigMainPyFile(StringExpression(x)) => notEmpty(x, s"$name.$MAINPYFILE")
      case _ =>
    }
  }
}
