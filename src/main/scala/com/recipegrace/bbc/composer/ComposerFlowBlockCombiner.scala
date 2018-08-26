package com.recipegrace.bbc.composer

import com.recipegrace.bbc.grmr.BBCStructures
import com.recipegrace.bbc.workflow.{FlowBlock, KeyAndContent, ProgramConfiguration}
import org.fusesource.scalate.TemplateEngine

import scala.collection.GenTraversableOnce

object ComposerFlowBlockCombiner {


  def generateBody(list: List[KeyAndContent]):String = {
    list.map(f=>f.content).mkString("", "\n","# End of declarations\n ") ++ list.filter(f=>f.createTask).map(f=> f.key).mkString("\n# Define DAG dependencies\n"," >> ", "\n#End of steps")
  }

  def mergeWorkflow(blocks: List[FlowBlock], findResources: List[BBCStructures.GitRepository], programConfiguration: ProgramConfiguration) = {

    generateHeaderContent(programConfiguration) ++ generateBody(blocks.flatMap(f=>f.template))
  }

   def generateHeaderContent(programConfiguration: ProgramConfiguration) = {
    Templates.translate("templates/workflow-start.ssp", Map("programConfiguration" -> programConfiguration))
  }
}
