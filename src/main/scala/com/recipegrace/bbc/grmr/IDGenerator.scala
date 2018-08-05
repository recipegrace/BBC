package com.recipegrace.bbc.grmr

/**
  * Created by Ferosh Jacob on 11/23/16.
  */
object IDGenerator {
  var count = 0

  def autoId = {
    count = count + 1
    count
  }

  def createUserTaskId(id: Int) = {
    "userTask" + id
  }

  def createServiceTaskId(id: Int) = {
    "serviceTask" + id
  }

  def createFlowId(id: Int) = {
    "flow_" + id
  }

  def createProcessId(id: Int) = {
    "process_" + id
  }

  def createBoundaryTimerId(id: Int): String = {
    "boundarytimer_" + id
  }

}
