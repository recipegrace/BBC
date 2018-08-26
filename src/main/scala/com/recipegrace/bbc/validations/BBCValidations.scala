package com.recipegrace.bbc.validations

/**
  * Created by Ferosh Jacob on 11/8/16.
  */

import com.recipegrace.bbc.grmr.BBC
import com.recipegrace.bbc.grmr.BBCStructures._
trait BBCValidations extends ClusterValidation with SparkJobValidation
  with PySparkJobValidation
  with SBTJobValidation
  with JavaJobValidation
  with ProgramConfigValidation with RepositoryValidation with PyJobValidation {


  def validateBBC(bBC: BBC) = {
    validateProgramConfigs(bBC.programConfigs)
    validateDistinctName( bBC.clusters, (x: Cluster) => x.name, "Cluster")
    validateDistinctName(bBC.sparkJobs, (x: SparkJob) => x.name, "SparkJob")
    validateDistinctName(bBC.pySparkJobs, (x: PySparkJob) => x.name, "PySparkJob")
    validateDistinctName(bBC.pyJobs, (x: PyJob) => x.name, "PyJob")
    validateDistinctName(bBC.sbtJobs, (x: SBTJob) => x.name, "SBTJob")
    validateDistinctName(bBC.javaJobs, (x: JavaJob) => x.name, "JavaJob")

    validateDistinctName(bBC.repositories, (x: Repository) => x.getName, "Nexus/Artifactory")

    bBC.clusters.foreach(validateCluster)
    bBC.sparkJobs.foreach(validateSparkJob)
    bBC.pySparkJobs.foreach(validatePySparkJob)
    bBC.pyJobs.foreach(validatePyJob)
    bBC.sbtJobs.foreach(validateSBTJob)
    bBC.javaJobs.foreach(validateJavaJob)

    bBC.repositories.foreach(f => validateRepository(f, bBC.programConfiguration))



    validateDeclarations(bBC.repositories.map(f=> f.getName),
      bBC.sparkJobs.map(f => f.name -> nexusInSparkJob(f)).flatMap(f => f._2.map(g=> f._1 ->g)), "Nexus/Artifactory", "SparkJob")

    validateDistinctName( bBC.allDeclarations,(x:(String, Declaration)) => x._1, "Declaration")
    validateActions(bBC: BBC, bBC.actions: List[ActionTypeWithId])

  }

  def validateSparkVariables(x: RunJobAction, job :Option[SparkJob]) = {
    job.foreach(sparkJob => assert(x.variables.size==sparkJob.variables.size, s"definition of spark job:${sparkJob.name} and usage have different parameters"))
    job.foreach(sparkJob => assert(x.variables.size==sparkJob.variables.distinct.size, s"definition variable in ${sparkJob.name} is repeated"))
  }
  def validatePySparkVariables(x: RunJobAction, job :Option[PySparkJob]) = {
    job.foreach(sparkJob => assert(x.variables.size==sparkJob.variables.size, s"definition of spark job:${sparkJob.name} and usage have different parameters"))
    job.foreach(sparkJob => assert(x.variables.size==sparkJob.variables.distinct.size, s"definition variable in ${sparkJob.name} is repeated"))
  }
  def validatePyJobVariables(x: RunJobAction, job :Option[PyJob]) = {
    job.foreach(pyJob => assert(x.variables.size==pyJob.variables.size, s"definition of pythonjob:${pyJob.name} and usage have different parameters"))
    job.foreach(pyJob => assert(x.variables.size==pyJob.variables.distinct.size, s"definition variable in ${pyJob.name} is repeated"))
  }
  def validateSBTJobVariables(x: RunJobAction, job :Option[SBTJob]) = {
    job.foreach(sbtJob => assert(x.variables.size==sbtJob.variables.size, s"definition of sbtjob:${sbtJob.name} and usage have different parameters"))
    job.foreach(sbtJob => assert(x.variables.size==sbtJob.variables.distinct.size, s"definition variable in ${sbtJob.name} is repeated"))
  }
  def validateJavaJobVariables(x: RunJobAction, job :Option[JavaJob]) = {
    job.foreach(javaJob => assert(x.variables.size==javaJob.variables.size, s"definition of javajob:${javaJob.name} and usage have different parameters"))
    job.foreach(javaJob => assert(x.variables.size==javaJob.variables.distinct.size, s"definition variable in ${javaJob.name} is repeated"))
  }


  def validateActions(bbc: BBC, actions: List[ActionTypeWithId]) = {
    def validateEachAction(x:ActionTypeWithId):Unit= {
      x match {
        case x: RunJobAction => {
          assert(bbc.allDeclarationsMap.contains(x.job), s"job:${x.job} is not defined")
          bbc.allDeclarationsMap(x.job) match {
            case y: SparkJob => {
              assert(x.cluster.nonEmpty, "Cluster name is not specified for job:" + x.job)
              validateSparkVariables(x, bbc.sparkJobs.find(f => f.name.equals(x.job)))
              validateLinks[RunJobAction](bbc.clusters.map(f => f.variableName), (x: RunJobAction) => x.cluster.get, x, "Cluster")

            }
            case y: PySparkJob => {
              assert(x.cluster.nonEmpty, "Cluster name is not specified for job:" + x.job)
              validatePySparkVariables(x, bbc.pySparkJobs.find(f => f.name.equals(x.job)))
              validateLinks[RunJobAction](bbc.clusters.map(f => f.variableName), (x: RunJobAction) => x.cluster.get, x, "Cluster")
            }
            case y: PyJob => {
              assert(x.cluster.isEmpty, s"Cluster ${x.cluster.get}  specified for a python job:" + x.job)
              validatePyJobVariables(x, bbc.pyJobs.find(f => f.name.equals(x.job)))
            }
            case y: SBTJob => {
              assert(x.cluster.isEmpty, s"Cluster ${x.cluster.get}  specified for a sbt job:" + x.job)
              validateSBTJobVariables(x, bbc.sbtJobs.find(f => f.name.equals(x.job)))
            }
            case y: JavaJob => {
              assert(x.cluster.isEmpty, s"Cluster ${x.cluster.get}  specified for a sbt job:" + x.job)
              validateJavaJobVariables(x, bbc.javaJobs.find(f => f.name.equals(x.job)))
            }
            case y:PipelineJob => {
              assert(x.cluster.isEmpty, s"Cluster ${x.cluster.get}  specified for a pipeline job:" + x.job)
              val pipeLineInsidePipelines=y.actions.flatMap {
                case f: RunJobAction =>
                  bbc.allDeclarationsMap.get(f.job) match {
                    case Some(job) => job match {
                      case a: PipelineJob => List(a)
                      case _ => List()
                    }
                    case _ => List()
                  }
                case _ => List()
              }
              assert(pipeLineInsidePipelines.isEmpty,s"Pipeline (${pipeLineInsidePipelines.head.name})  inside pipeline (${y.name}) not allowed!")
              y.actions.foreach(validateEachAction)
            }
            case _ => {

            }

          }

        }
        case _ => None
      }
    }
    assert(actions.nonEmpty, "BBC should have at least one action")

    actions.foreach(validateEachAction)
  }

  def validateLinks[Q](allNames:List[String], getLink: (Q) => String, link: Q, entity: String) = {
    val tobeChecked = getLink(link)
    assert(allNames.contains(tobeChecked), s"$entity name is not defined: $tobeChecked")
  }

  def validateDeclarations(legalNames: List[String], tuples: List[(String, String)], s1: String, s2: String) = {

    tuples.foreach(f => {
      assert(legalNames.contains(f._2), s"$s1 ${f._2} not defined on $s2 ${f._1}")
    })
  }

  def nexusInSparkJob(sparkJob: SparkJob): Option[String] = {
    val nexus = sparkJob.sparkJobConfigs.filter(f => f match {
      case x: RepositoryJarURI => true
      case _ => false
    }).map(f => f.asInstanceOf[RepositoryJarURI]).map(f => f.nexusKey)
    nexus match {
      case x :: List() => Some(x)
      case _ => None
    }

  }
}
