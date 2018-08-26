package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.grmr.Expressions._
import com.recipegrace.bbc.grmr.IDGenerator._

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
case class ProgramConfiguration(env: Expression, zone: Expression, name: String, cloudWorkspace: Option[Expression])
case class KeyAndContent(key:String, content:String,createTask:Boolean)

abstract class FlowBlock(programConfig: ProgramConfiguration) {


  var toXML: NodeSeq

  var displayName:String
  val flowBlockId:Int
  var toYAML:List[Map[String,Any]]
  var _startID = ""
  var _endID = ""

  def startID: String = {
    if (_startID == "") toXML
    assert(_startID != "", s"wrong or early access startID empty")
    _startID
  }

  def endID: String = {
    if (_endID == "") toXML
    assert(_endID != "", s"wrong or early access endID empty ")
    _endID
  }

  def createConnect(from: Int, to: Int): NodeSeq = {
    <sequenceFlow id={createFlowId(autoId)} sourceRef={createServiceTaskId(from)} targetRef={createUserTaskId(to)}></sequenceFlow>
  }

  override def toString = {
    s" ${this.getClass.getSimpleName}: [${_startID} -> ${_endID}]"
  }

  def template:List[KeyAndContent] = List()
}
