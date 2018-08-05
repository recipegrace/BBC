package com.recipegrace.bbc.codegen

import com.recipegrace.bbc.grmr.Expressions.{Expression, StringExpression}


/**
  * Created by Ferosh Jacob on 11/8/16.
  */
trait GCSBucketServiceTask {


  def deleleGCSLocationServiceTask(location: Expression, id: Int,baseTask: BaseTask) = {


    baseTask.createBaseServiceTask(id, "DeleteGCSLocationTask" + id,
      "com.recipegrace.bbd.main.DeleteCloudLocationEntryPoint", List("-cloudLocation" -> location))


  }


}
