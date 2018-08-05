package com.recipegrace.bbc.grmr

import com.recipegrace.bbc.activiti.ActivitiFlowBlockCombiner
import com.recipegrace.bbc.grmr.Expressions.StringExpression

/**
  * Created by Ferosh Jacob on 11/8/16.
  */
class BBCActionsTest extends BaseBBCGrammarTest  {

  test("delete location test") {
    val location = "gs://hsds/sdd"
    val code = s"""delete "$location" """
    val delete = parseAll(_deleteAction, code)
    delete.get.folder shouldBe StringExpression (location)

  }
  test("run action test") {
    val cluster = "clusterName"
    val jobName = "jobname"
    val code = s"run $jobName on $cluster"
    val run = parseAll(_runJobAction, code)
    run.get.cluster.get shouldBe cluster
    run.get.job shouldBe jobName

  }


  test("run action test without cluster") {
    val jobName = "jobname"
    val code = s"run $jobName"
    val run = parseAll(_runJobAction, code)
    run.get.job shouldBe jobName

  }




}
