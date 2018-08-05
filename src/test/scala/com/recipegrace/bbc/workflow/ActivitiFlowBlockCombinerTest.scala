package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.BaseTest
import com.recipegrace.bbc.activiti.ActivitiFlowBlockCombiner
import com.recipegrace.bbc.codegen.ExpressionCreator

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/25/16.
  */
class ActivitiFlowBlockCombinerTest extends BaseTest   with ExpressionCreator{


  test("flowblock check") {
    val flowBlock1 = new FlowBlock(ProgramConfiguration(expr(""), expr(""), "", None)) {
      override var toXML: NodeSeq = <x/>
      override var displayName: String = "s"
      override val flowBlockId: Int = 1
      override var toYAML: List[Map[String, Any]] = List()
    }
    intercept[AssertionError](ActivitiFlowBlockCombiner.mergeFlows(List(flowBlock1))).getMessage shouldBe "assertion failed: wrong or early access startID empty"
  }
  test("flow block combiner test") {

    def format(nodes: NodeSeq) = {

      val xml = <test>
        {nodes}
      </test>

    }

    val outputExpected =
      <sequenceFlow id="flow_2" sourceRef="theStart" targetRef="a"></sequenceFlow>
          <x/>
          <y/>
        <sequenceFlow id="flow_1" sourceRef="b" targetRef="c"></sequenceFlow>
        <sequenceFlow id="flow_3" sourceRef="d" targetRef="theEnd"></sequenceFlow>

    val block1 = new FlowBlock(new ProgramConfiguration(expr(""), expr(""), "", None)) {
      override var toXML: NodeSeq = <x/>
      _startID = "a"
      _endID = "b"
      override var displayName: String = "s"
      override val flowBlockId: Int = 1
      override var toYAML: List[Map[String, Any]] = List()
    }
    val block2 = new FlowBlock(ProgramConfiguration(expr(""), expr(""), "", None)) {
      override var toXML: NodeSeq = <y/>
      _startID = "c"
      _endID = "d"
      override var displayName: String = "s"
      override val flowBlockId: Int = 1
      override var toYAML: List[Map[String, Any]] = List()
    }
    format(ActivitiFlowBlockCombiner.mergeFlows(List(block1, block2))) shouldBe format(outputExpected)
  }
}
