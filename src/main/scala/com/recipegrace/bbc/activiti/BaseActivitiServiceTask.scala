package com.recipegrace.bbc.activiti

import com.recipegrace.bbc.codegen.BaseTask
import com.recipegrace.bbc.grmr.Expressions._
import com.recipegrace.bbc.grmr.IDGenerator._

import scala.xml.{NodeSeq, Text}

/**
  * Created by Ferosh Jacob on 11/8/16.
  */
object BaseActivitiServiceTask extends BaseTask {



  override def createBaseServiceTask(id: Int, name: String, className: String, properties: List[(String, Expression)]):NodeSeq = {

    val userTaskId = createUserTaskId(id)
    val serviceTaskId = createServiceTaskId(id)
    val boundaryTimerId = createBoundaryTimerId(id)

    <userTask id={userTaskId} name={"UT" + name}></userTask>
      <boundaryEvent id={boundaryTimerId} name={"Timer" + name} attachedToRef={userTaskId} cancelActivity="true">
        <timerEventDefinition>
          <timeDuration>PT1S</timeDuration>
        </timerEventDefinition>
      </boundaryEvent>
      <serviceTask name={name} activiti:class={className} id={serviceTaskId}>
        <extensionElements>
          {properties.map(f => {
            f._2 match {
              case VariableExpression(variableName) => {
                <activiti:field name={f._1}>
                  <activiti:expression>{Text(s"$${$variableName}")}</activiti:expression>
                </activiti:field>
                  }
              case StringExpression(value) => {
                  <activiti:field name={f._1} stringValue={value}/>
              }
              case NumberExpression(value) => {
                  <activiti:field name={f._1} stringValue={value+""}/>
              }
              case x:ConcatStringExpression => {
                  <activiti:field name={f._1} stringValue={x.value+""}/>
              }
            }
        })}
        </extensionElements>
      </serviceTask>
      <sequenceFlow id={createFlowId(autoId)} sourceRef={userTaskId} targetRef={serviceTaskId}></sequenceFlow>
      <sequenceFlow id={createFlowId(autoId)} sourceRef={boundaryTimerId} targetRef={serviceTaskId}></sequenceFlow>
  }
}
