package com.recipegrace.bbc.concourse

import com.recipegrace.bbc.BaseTest

/**
  * Created by Ferosh Jacob on 2/4/17.
  */
class YAMLGeneratorTest extends BaseTest{

  def compareYAML(input:Any, expected:String) = {
   val actual= YAMLGenerator.generate(input)
    println(actual+"\n.")
    actual shouldBe expected
  }

  test("yaml serialization test") {

    val name ="ferosh"
    val org ="recipegrace"
    val map = Map("name"-> s"$name", "org" -> s"$org")

    val map1 = List( map,  List("one", "two"))

   compareYAML(map, s"---\nname: $name\norg: $org\n")
   compareYAML(map1,"---\n- name: ferosh\n  org: recipegrace\n- - one\n  - two\n")

    val map2 = Map("map"-> List(map))
    compareYAML(map2, "---\nmap:\n- name: ferosh\n  org: recipegrace\n")
  }

  test("actual pipeline test") {
    val input = Map( "resources" -> List( Map ("name"-> "bb-jar-repo", "type"-> "git","check_every"-> "10s","source" ->
      Map("uri"->"git@github.recipegrace.com:OLT-search/BigBricks-jar.git", "branch"->"master","private_key"-> "{{github-private-key}}"))
      ) )
    compareYAML(input,"---\nresources:\n- name: bb-jar-repo\n  type: git\n  check_every: 10s\n  source:\n    uri: git@github.recipegrace.com:OLT-search/BigBricks-jar.git\n    branch: master\n    private_key: {{github-private-key}}\n")
  }
}
