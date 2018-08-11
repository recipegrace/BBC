package com.recipegrace.bbc.concourse

trait ResourceNameGenerator {


  private def nonLetterToHyphen(x:String) = {
    x.map(f=> f match {
      case i if i.isLetterOrDigit => i.toLower
      case _ => '-'
    })
  }
  def createResourceName(repository:String, branch:String) = {

    nonLetterToHyphen(repository)+nonLetterToHyphen(branch)
  }

}
