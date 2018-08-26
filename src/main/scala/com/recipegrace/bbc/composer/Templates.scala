package com.recipegrace.bbc.composer

import org.fusesource.scalate.TemplateEngine

object Templates {

  val engine = new TemplateEngine()


  def translate(templateName:String, variables:Map[String, Any]):String = {
    engine.layout(templateName,variables)
  }
}
