package com.recipegrace.bbc.workflow

import java.io.Reader

import com.recipegrace.bbc.codegen.ExpressionCreator
import com.recipegrace.bbc.grmr.{BBCGrammar, BBCStructures}
import com.recipegrace.bbc.grmr.BBCStructures.{ClusterStore, VariableDeclarations}
import com.recipegrace.bbc.{ActivitiMain, ConcourseMain}
import com.recipegrace.bbc.BaseTest
import org.activiti.engine.ProcessEngineConfiguration

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/23/16.
  */
object ProcessEngineObject {
  val processEngine = ProcessEngineConfiguration
    .createStandaloneInMemProcessEngineConfiguration()
    .buildProcessEngine()
}

trait BaseWorkflowTest extends BaseTest with BBCGrammar with ExpressionCreator {

  val nameZoneStage =
    """
        name="1"
        zone="east"
        env="stage"
    """
  val anAction =
    """
      delete "gs://sd/sds"
    """.stripMargin

  def dsl(clusterName: Boolean, sparkJob: Boolean) =
    s"""
                      name="hello"
                      env="stage"
                      zone="east"
                      cluster simpleCluster {
                            workers=2
                            image="simpleimage"
                            properties="example properties"
                            }
                      sparkjob sparkJobName {
                            mainClass="className"
                            args= "sdsd"
                            props="sparkProps"
                            jarLocation="jarURI"
                            }
                      run sparkJobName${enable(sparkJob, "1")} on  simpleCluster${enable(clusterName, "1")}

            """

  def enable(flag: Boolean, text: String) = {
    if (flag) text
    else ""
  }

  def createActivitWorkFlow(dsl: String) = {

    ActivitiMain.generateProcess(dsl)
  }
  def createConcourseWorkFlow(dsl: String) = {

    ConcourseMain.generateProcess(dsl)
  }

  def isValidActivitiWorkflow(dsl: String) = {
    val bbc = parseAll(_bbc, dsl).get
    val xml = GenerateFlows(bbc).generateActiviti
    isValidWorkFlow(xml)
  }
  def isValidConcourseWorkflow(dsl: String) = {
    val yaml = ConcourseMain.generateProcess(dsl)
    yaml.get.nonEmpty shouldBe true
    println(yaml)
    ConcourseMain.generateFlows.clusterStore.running shouldBe List()
    ConcourseMain.errorMessage shouldBe ""

  }
  def getKeyValuePairs(dsl:String) = {
    val bbc = parseAll(_bbc, dsl).get
    GenerateFlows(bbc).generateConcourse
  }
def generateYAMLObject(dsl:Reader) = {
    BBCStructures.variableStore= Map()
      val bbc = parseAll(_bbc, dsl).get
    GenerateFlows(bbc).generateConcourse
  }



  def isValidWorkFlow(xml: NodeSeq) = {
    //   val workflow= intercept[ActivitiException](deployToCheckTheXMLisValid(xml))
    // workflow.getMessage shouldBe "couldn't instantiate class com.recipegrace.bbd.delegates.dataproc.DataProcClusterCreatorBlocking"
    deployToCheckTheXMLisValid(xml)
  }

  def deployToCheckTheXMLisValid(xml: NodeSeq, fileName: String = "ExampleProcess.bpmn20.xml") = {
    import ProcessEngineObject._
    val repositoryService = processEngine.getRepositoryService()

    val processId = (xml \\ "process" \ "@id").text
    // Deploy the process definition
    repositoryService.createDeployment()
      .addString(fileName, xml.toString())
      .deploy()

  }

  def assertionErrorOnDeclaration(x: String, message: String) = {

    assertionError(nameZoneStage + x + anAction, message)
  }

  def getFlowblocks(x: String) = {
    val dsl =  nameZoneStage + x + anAction
    val yaml = ConcourseMain.generateProcess(dsl)
    yaml.get.nonEmpty shouldBe true
    ConcourseMain.generateFlows.clusterStore.running shouldBe List()
    BBCStructures.variableStore=Map()
    val content = parseAll(_bbc, dsl)
    val bbc = content.get

    GenerateFlows(bbc).generateConcourse

  }

  def newClustersCreated(x:String) = {
    getFlowblocks(x)("jobs").head("plan").asInstanceOf[List[Map[String, Any]]].count(f => f("task").toString.startsWith("CreateCluster"))
  }

  def getInputs(x:String) = {
    getFlowblocks(x)("jobs").head("plan").asInstanceOf[List[Map[String, Any]]].flatMap(f => {

      val taskOrInput = f.asInstanceOf[Map[String,Any]]
      taskOrInput.get("get") match {
        case Some(x) => List(x)
        case _ => List()
      }
    })
  }
  def assertionError(x: String, message: String) = {
    BBCStructures.variableStore=Map()
    val content = parseAll(_bbc, x)
    val bbc = content.get


    val yaml = intercept[AssertionError](GenerateFlows(bbc).generateConcourse)
    yaml.getMessage shouldBe "assertion failed: " + message
    val xml = intercept[AssertionError](GenerateFlows(bbc).generateActiviti)
    xml.getMessage shouldBe "assertion failed: " + message
  }
  def assertionErrorOnDeclarationYAML(x: String, message: String) = {

    assertionErrorYAML(nameZoneStage + x + anAction, message)
  }

  def assertionErrorYAML(x: String, message: String) = {
    BBCStructures.variableStore=Map()
    val content = parseAll(_bbc, x)
    val bbc = content.get
    val yaml = intercept[AssertionError](GenerateFlows(bbc).generateConcourse)

    yaml.getMessage shouldBe "assertion failed: " + message
  }
  def assertionError(x: String, parser:Parser[_], message: String) = {
    //val xml = intercept[AssertionError]
    (parseAll(parser, x))
 //  printn(xml)
    // xml.getMessage shouldBe "assertion failed: " + message
  }

  def assertErrorOnParser[T](method: Parser[T], message:String,dsl:String) = {
    val xml = intercept[AssertionError] (parseAll(method, dsl))
    xml.getMessage shouldBe "assertion failed: " + message
  }
}
