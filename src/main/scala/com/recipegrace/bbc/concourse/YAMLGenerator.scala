package com.recipegrace.bbc.concourse

import java.io.StringWriter

import org.yaml.snakeyaml.{DumperOptions, Yaml}

import scala.collection.JavaConverters._
/**
  * Created by Ferosh Jacob on 2/4/17.
  */
object YAMLGenerator {

  val options = new DumperOptions {

    override def calculateScalarStyle(analysis: _root_.org.yaml.snakeyaml.emitter.ScalarAnalysis, style: _root_.org.yaml.snakeyaml.DumperOptions.ScalarStyle): _root_.org.yaml.snakeyaml.DumperOptions.ScalarStyle =
      DumperOptions.ScalarStyle.PLAIN
  }
  options.setExplicitStart(true)

  options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
  val yaml = new Yaml(options)
  private def toYAML(input:Any):Any = {
    input match {
      case x: String => {
        x
      }
      case x: List[Any] => {
        x.map(toYAML).asJava
      }
      case x: Map[String, Any] => {
        x.mapValues(toYAML).asJava
      }
      case _ => {
        assert(false, s"toYAML not supported for $input")
      }
    }

  }

  def generate(input:Any) = {
    yaml.dump(toYAML(input))
  }
}
