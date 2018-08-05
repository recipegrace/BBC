package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait ProgramConfigValidation extends BaseValidations with GrammarKeywords {

  def validateProgramConfigs(programConfigs: List[ProgramConfig]) = {

    mandatory(List(ENV, SIMPLENAME, ZONE), programConfigs)
    configValidation(programConfigs)
    programConfigs.foreach {
      case ProgramConfigEnv(StringExpression(x)) => notEmpty(x, "program environment")
      case ProgramConfigZone(StringExpression(x)) => notEmpty(x, "program zone")
      case ProgramConfigName(x) => notEmpty(x, "program name")
      case _ =>
    }
  }


}
