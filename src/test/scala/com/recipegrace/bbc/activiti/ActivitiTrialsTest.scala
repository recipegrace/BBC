package com.recipegrace.bbc.activiti

import java.util
import scala.collection.JavaConversions._
import com.recipegrace.bbc.{BaseTest, ActivitiMain}
import com.recipegrace.bbc.workflow.ProcessEngineObject._
/**
  * Created by Ferosh Jacob on 12/27/16.
  */
class ActivitiTrialsTest extends BaseTest {



  test("delete folder and test") {
    val xml =ActivitiMain.generateProcess(
      """
        name= "delete folder"
        zone = "us-east1-c"
        env = "dev"
        var folderName="gs://mosambi/isarelations.json"
        delete folderName
      """.stripMargin).get
    val processId = (xml \\ "process" \ "@id").text
    // Deploy the process definition
    processEngine.getRepositoryService.createDeployment()
      .addString("ExampleProces.bpmn20.xml", xml.toString())
      .deploy()

    val map = new util.HashMap[String, AnyRef]()
    map.put("folderName", "gs://mosambi/projects.json")
    println(xml)
    val process = processEngine.getRuntimeService.startProcessInstanceByKey(processId, map)

  }
  test("check expression works") {

    val xml:String = """
 <definitions id="definitions" targetNamespace="http://activiti.org/bpmn20" xmlns="http://www.omg.org/spec/BPMN/20100524/MODEL" xmlns:omgdi="http://www.omg.org/spec/DD/20100524/DI" xmlns:omgdc="http://www.omg.org/spec/DD/20100524/DC" xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI" xmlns:activiti="http://activiti.org/bpmn">
      <process id="process5" name="test">
        <startEvent id="theStart" name="Start"></startEvent>
        <endEvent id="theEnd" name="End"></endEvent>
        <sequenceFlow id="flow_6" sourceRef="theStart" targetRef="serviceTask2"></sequenceFlow>

      <serviceTask name="DeleteGCSLocationTask2" activiti:class="com.recipegrace.bbc.activiti.ExampleDelegate" id="serviceTask2">
        <extensionElements>
          <activiti:field name="url">
            <activiti:expression>${customerNameVar}${"hello"}${customerNameVar}</activiti:expression>
           </activiti:field>
        </extensionElements>
       </serviceTask>
     <sequenceFlow id="flow_7" sourceRef="serviceTask2" targetRef="theEnd"/>
      </process>
    </definitions>"""


    deployAndTest(xml)



  }

  private def deployAndTest(xml: String) = {
    val repositoryService = processEngine.getRepositoryService()

    val processId = "process5"
    // Deploy the process definition
    val deployment = repositoryService.createDeployment()
      .addString("ExampleProces.bpmn20.xml", xml.toString())
      .deploy()


    val map = new util.HashMap[String, Object]()
    map.put("customerNameVar", "hello1")
    val process = processEngine.getRuntimeService.startProcessInstanceByKey(processId, map)

    processEngine.getHistoryService.createHistoricProcessInstanceQuery()
      .includeProcessVariables().processInstanceId(process.getProcessInstanceId).singleResult().getProcessVariables.toList
      .foreach(println)
  }
}
