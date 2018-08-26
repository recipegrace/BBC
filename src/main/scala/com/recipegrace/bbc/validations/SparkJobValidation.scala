package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait SparkJobValidation extends BaseValidations with GrammarKeywords {

  def validateSparkJob(sparkJob: SparkJob) = {
    validateSparkJobConfigs(sparkJob.sparkJobConfigs, sparkJob.name)
  }

  def validateSparkJobConfigs(sparkJobConfigs: List[BaseSparkJobConfig], name: String) = {
    mandatory(List(MAINCLASS, JARLOCATION), sparkJobConfigs, s"SparkJob $name")
    configValidation(sparkJobConfigs, s"on SparkJob $name")
    sparkJobConfigs.foreach {
      case SparkJobConfigClassName(StringExpression(x)) => {
        notEmpty(x, s"$name.mainClass")
      }
      case SparkJobConfigJarURI(StringExpression(x)) => notEmpty(x, s"$name.jarURI")
      case RepositoryJarURI(StringExpression(x),StringExpression(y),StringExpression(z),_) => {
        notEmpty(x,s"$name.jarLocation.organization")
        notEmpty(y,s"$name.jarLocation.artifactId")
        notEmpty(z,s"$name.jarLocation.version")

      }
      case SparkJobConfigProps(StringExpression(x)) => notEmpty(x, s"$name.sparkJobProperties")
      case _ =>
    }
  }
}
