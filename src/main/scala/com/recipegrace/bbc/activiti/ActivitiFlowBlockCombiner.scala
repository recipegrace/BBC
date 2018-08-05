package com.recipegrace.bbc.activiti

import java.util.logging.Logger

import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.workflow.FlowBlock

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/23/16.
  */
object ActivitiFlowBlockCombiner {
  val logger = Logger.getLogger(this.getClass.getName)

  val startFlowId = "theStart"
  val endFlowId = "theEnd"

  def wrapWorkflow(flowBlocks: List[FlowBlock], name: String) = {

    <definitions id="definitions"
                 targetNamespace="http://activiti.org/bpmn20"
                 xmlns:activiti="http://activiti.org/bpmn"
                 xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
                 xmlns:omgdc="http://www.omg.org/spec/DD/20100524/DC"
                 xmlns:omgdi="http://www.omg.org/spec/DD/20100524/DI"
                 xmlns="http://www.omg.org/spec/BPMN/20100524/MODEL">

      <process id={"process" + autoId} name={name}>
        <startEvent id={startFlowId} name="Start"></startEvent>
        <endEvent id={endFlowId} name="End"></endEvent>{mergeFlows(flowBlocks)}
      </process>
    </definitions>
  }


  def mergeFlows(blocks: List[FlowBlock]): NodeSeq = {

    val firstBlock = blocks.head
    val (content, lastBlock) = blocks.tail.foldLeft((NodeSeq.Empty, blocks.head): (NodeSeq, FlowBlock))((p, q) => {
      (p._1 ++ connect(p._2, q), q)
    })
    createStartFlow(firstBlock) ++ blocks.foldLeft(NodeSeq.Empty)((p, q) => p ++ q.toXML) ++ content ++ createEndFlow(lastBlock)
  }

  def createEndFlow(x: FlowBlock) = {
    <sequenceFlow id={createFlowId(autoId)} sourceRef={x.endID} targetRef={endFlowId}></sequenceFlow>
  }

  def createStartFlow(x: FlowBlock) = {
    <sequenceFlow id={createFlowId(autoId)} sourceRef={startFlowId} targetRef={x.startID}></sequenceFlow>

  }

  def connect(from: FlowBlock, to: FlowBlock): NodeSeq = {

    assert(from.startID != to.startID && from.endID != to.endID, s"Both are same $from ->$to")
    <sequenceFlow id={createFlowId(autoId)} sourceRef={from.endID} targetRef={to.startID}></sequenceFlow>

  }


}
