package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait SBTJobValidation extends BaseValidations with GrammarKeywords {

  def validateSBTJob(sbtJob: SBTJob) = {

    validateSBTJobConfigs(sbtJob.sbtJobConfigs, sbtJob.name)
  }

  def validateSBTJobConfigs(sbtJobConfigs: List[RepositoryJobConfig], name: String) = {
    mandatory(List(MAINCLASS)++repositoryMandatoryFields, sbtJobConfigs, s"sbtjob $name")
    validateRepositoryJobConfigs(sbtJobConfigs,name)
    configValidation(sbtJobConfigs, s"on sbtjob $name")
    sbtJobConfigs.foreach {
      case SBTJobConfigMainClass(StringExpression(x)) => notEmpty(x, s"$name.$MAINCLASS")
      case _ =>
    }
  }
}
