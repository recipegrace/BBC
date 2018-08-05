package com.recipegrace.bbc.concourse

import com.recipegrace.bbc.codegen.BaseTask
import com.recipegrace.bbc.grmr.Expressions.{Expression, StringExpression}

/**
  * Created by Ferosh Jacob on 2/4/17.
  */
object BaseConcourseDelegateTask  extends BaseTask{


  override def createBaseServiceTask(id: Int, name: String, className: String, properties: List[(String, Expression)]) = {

    /*
            - -proxyURL
        - {{proxy-url}}
        - -proxyPort
        - {{proxy-port}}
        - -credentialFile
        - {{gcp-token}}
     */
    val defaultProperties = List( "-proxyURL", "{{proxy-url}}", "-proxyPort","{{proxy-port}}", "-credentialFile", "{{gcp-token}}")
    val propList = properties
      .map(f=> f._1-> f._2.value.toString)
      .flatMap(f=> List(f._1,f._2))


    val configs= Map(
      "image_resource" -> Map("type"->"docker-image", "source"-> Map("repository"->"recipegrace/bigbricks-docker")),
      "platform"->"linux",
      "run" -> Map("path"->"java","args" -> ( List("-cp","/usr/local/bigbricks/bigbricks-assembly.jar",className)++defaultProperties ++ propList))
    )
     Map("task" -> (name+id), "config" -> configs)
  }

  }
