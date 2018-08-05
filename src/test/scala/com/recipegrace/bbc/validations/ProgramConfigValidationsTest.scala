package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCGrammar
import com.recipegrace.bbc.workflow.BaseWorkflowTest


/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class ProgramConfigValidationsTest extends BaseWorkflowTest with ClusterValidation with BBCGrammar {


  test("empty") {


    assertionError(
      """
        name=""
        zone="zone"
        env="env"
        delete "gs://sd/ss"
      """.stripMargin, "program name cannot be empty")

    assertionError(
      """
        name="name"
        zone=""
        env="env"
        delete "gs://sd/ss"
      """.stripMargin, "program zone cannot be empty")

    assertionError(
      """
        name="name"
        zone="zone"
        env=""
        delete "gs://sd/ss"
      """.stripMargin, "program environment cannot be empty")
  }

  test("repeated") {

    for (content <- List("name", "zone", "env")) {
      assertionError(
        s"""
        name="name"
        zone="zone"
        env="dev"
        $content="$content"
        delete "gs://sd/ss"
      """.stripMargin, content + " have repeated declaration")
    }
  }

  test("mandatory") {

    assertionError(
      "".stripMargin, s"BBC have missing field(s):env,name,zone")
    assertionError(
      """
        name ="name"
        zone = "zone"
        env = "env"
      """.stripMargin, s"BBC should have at least one action")
  }
}
