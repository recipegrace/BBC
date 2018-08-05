package com.recipegrace.bbc.grmr


/**
  * Created by Ferosh Jacob on 11/6/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.{NumberExpression, StringExpression}

class BBCClusterTest extends BaseBBCGrammarTest {


  test("name  zone and env") {
    val name = "recipegrace"
    val env = "QA"
    val zone = "zone"
    val emptyContent =
      s"""
      zone="$zone"
      env="$env"
      name="$name"
      """
    val bbc = parseAll(_bbc, emptyContent).get

    bbc.programConfigs.foreach {
      case ProgramConfigEnv(StringExpression(x)) => x shouldBe env
      case ProgramConfigName(x) => x shouldBe name
      case ProgramConfigZone(StringExpression(x)) => x shouldBe zone
      case _ =>
    }

  }

  test("name  zone and env and cloud workspace") {
    val name = "recipegrace"
    val env = "QA"
    val zone = "zone"
    val cloudworkspace = "cloudworkspace"
    val emptyContent =
      s"""
      zone="$zone"
      env="$env"
      name="$name"
      cloudWorkspace="$cloudworkspace"
      """
    val bbcs = parseAll(_bbc, emptyContent)
    println(bbcs)

    bbcs.get.programConfigs.foreach {
      case ProgramConfigEnv(StringExpression(x)) => x shouldBe env
      case ProgramConfigName(x) => x shouldBe name
      case ProgramConfigZone(StringExpression(x)) => x shouldBe zone
      case ProgramConfigCloudWorkspace(StringExpression(x)) => x shouldBe cloudworkspace
      case _ =>
    }

  }
  test("cluster test") {
    val clusterName = "datacluster"
    val workers = 0
    val image = "sdd-hs-12"
    val properties = "properties"

    val clusterBlock =
      s"""
      cluster ${clusterName} {
      workers=${workers}
      image="${image}"
      properties="${properties}"
      }
    """


    val cluster = parseAll(_clusterBody, clusterBlock).get


    cluster._2.clusterConfigs.foreach {
      case ClusterConfigWorkers(NumberExpression(x)) => x shouldBe workers
      case ClusterConfigImage(StringExpression(x)) => x shouldBe image
      case ClusterConfigProperties(List(StringExpression(x))) => x shouldBe properties
    }
  }
}
