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
object ConcourseMain extends BaseTemplateMain {


  override def onSuccessParse(result: BBC) = {
    generateFlows = GenerateFlows(result)
    val bbcInput = generateFlows.generateConcourse
    Some(YAMLGenerator.generate(bbcInput))
  }

}
