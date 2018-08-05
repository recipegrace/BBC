package com.recipegrace.bbc.concourse

import java.util.logging.Logger

import com.recipegrace.bbc.grmr.BBCStructures.GitRepository
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.workflow._
import com.recipegrace.bbc.workflow.{ArtifactoryDownloadFlowBlock, FlowBlock, NexusDownloadFlowBlock}

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/23/16.
  */
object ConcourseFlowBlockCombiner {
  val logger = Logger.getLogger(this.getClass.getName)


  /*
  - name: run-sparkjob
  serial: true
  plan:
  - task: nexus-to-cloud
   */

  def getBlockName(f:FlowBlock):String = {
    f.displayName+f.flowBlockId.toString
  }
  def convertToConCourseJob(f: FlowBlock) = {

    Map("name"-> getBlockName(f),"serial"->"true", "plan"->f.toYAML)
  }

  def convertToConcourseResources(f:List[GitRepository]) = {

    f.map(f=> Map("name"-> (f.name+"-resource" ), "type" -> "git", "source" -> Map ("uri"->f.repository, "branch" -> f.branch,
      "private_key" ->"{{github-private-key}}"), "check_every"->"24h","webhook_token"->"oltsearchwebhooktoken" ))
  }
  def convertToConCourseJob(f: List[FlowBlock],name:String,gitResources:List[GitRepository]) = {

    val yaml = gitResources.map(f=>  Map("get" -> (f.name+"-resource"), "trigger" ->"false"))++f.flatMap(g=>g.toYAML)
    Map("name"-> name,"serial"->"true", "plan"->yaml)
  }


  def mergeYAML(flowBlocks: List[FlowBlock], gitResources:List[GitRepository], name: String) = {



    val sparkJobs = flowBlocks.filter(f=> f match {
      case x:NexusDownloadFlowBlock => false
      case x:ArtifactoryDownloadFlowBlock =>false
      case _ => true
    })
    val otherJobs = flowBlocks.flatMap(f=> f match {
      case x:NexusDownloadFlowBlock => List(convertToConCourseJob(f))
      case x:ArtifactoryDownloadFlowBlock => List(convertToConCourseJob(f))
      case _ => List()
    })
    Map ("resources" -> convertToConcourseResources(gitResources),"jobs" ->  (convertToConCourseJob(sparkJobs,name,gitResources) :: otherJobs))
  }





}
