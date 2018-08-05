package com.recipegrace.bbc.codegen

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/8/16.
  */
class GCSBucketServiceTaskTest extends BaseCodeGenTest with GCSBucketServiceTask {

  val id = System.currentTimeMillis().toInt
  val stringLocation = StringExpression( "gs:/sdsds/g")
  val expectedExpression = Map(
    "-cloudLocation" -> stringLocation
  )
  def deleteCloudLocation(baseTask: BaseTask) = {

      deleleGCSLocationServiceTask(stringLocation, id, baseTask)
  }

  test("delete gcs location (activiti)") {

  val xmlContent = deleteCloudLocation(BaseActivitiServiceTask).asInstanceOf[NodeSeq]
      (xmlContent \\ "serviceTask" \ "@name").text shouldBe "DeleteGCSLocationTask" + id
      (xmlContent \\ "serviceTask" \ "@id").text shouldBe "serviceTask" + id.toString
      fieldExpressionValueTest(xmlContent,expectedExpression )

    }
  test("delete gcs location (concourse)") {

    val map = deleteCloudLocation(BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]
    map("task") shouldBe "DeleteGCSLocationTask" + id +id

    val expected = expectedExpression.map(f=> f._1-> f._2.value.toString)

    fieldValueTest(map,expected)
  }

}
