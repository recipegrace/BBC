package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.BaseTest
import com.recipegrace.bbc.codegen.ExpressionCreator
import com.recipegrace.bbc.concourse.{ConcourseFlowBlockCombiner, ResourceNameGenerator}
import com.recipegrace.bbc.grmr.BBCStructures.{ClusterStore, GitRepository, PyJob}
import com.recipegrace.bbc.grmr.BaseBBCGrammarTest

class ConcourseFlowCombinerTest extends BaseBBCGrammarTest with ExpressionCreator with ResourceNameGenerator{



  test("resource name generator") {
    createResourceName("git@github.recipegrace.com:OLT-search/Judas.git", "feature/develop") shouldBe "git-github-recipegrace-com-olt-search-judas-gitfeature-develop"
    createResourceName("hik.com:sds", "sdsome-sds") shouldBe "hik-com-sdssdsome-sds"
  }

  test("simple case"){


    val pythonJob = """  pythonjob example {
       mainPyFile = "src/two_way_similarity.py"
       repository = "git@github.recipegrace.com:OLT-search/Judas.git"
       repoBranch = "master"
       container = "ksrpk/python-gensim-gcloud"
       args = "--this", "will", "--may", "work"

    }
    """

   val pyJob =  parseAll(_pyJobBody,pythonJob).get._2
   val pyBlock = new RunPythonFlowBlock(pyJob,Map(),ProgramConfiguration(expr("hello"),expr("zone"),"test",None),new ClusterStore(List()))

    makeSure(ConcourseFlowBlockCombiner.mergeYAML(List(pyBlock),List(GitRepository("repo","branch","name")),"hello"))


    makeSure(ConcourseFlowBlockCombiner.mergeYAML(List(pyBlock),List(GitRepository("repo","branch","name"),GitRepository("repo","branch","name")),"hello"))


    val result3 = ConcourseFlowBlockCombiner.mergeYAML(List(pyBlock, pyBlock),List(GitRepository("repo","branch","name"),GitRepository("repo","branch","name")),"hello")
    result3 should have size 2
    result3("resources").size shouldBe 1
  }

  private def makeSure(result: Map[String, List[Map[String, Object]]]) = {
    result should have size 2
    result("resources").size shouldBe 1
    result("resources").head("name") shouldBe "repobranch"
    val resourceName = result("jobs").head("plan").asInstanceOf[List[Map[String, Object]]].head("get")
    resourceName shouldBe "repobranch"
  }
}
