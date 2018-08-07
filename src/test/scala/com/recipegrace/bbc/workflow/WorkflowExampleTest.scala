package com.recipegrace.bbc.workflow

/**
  * Created by Ferosh Jacob on 11/23/16.
  */
class WorkflowExampleTest extends BaseWorkflowTest {


  test("create simple workflow test") {
    val xml = createActivitWorkFlow(dsl(false, false))
    isValidWorkFlow(xml.get)


  }

  test("only delete") {
    val dsl =
      """name="exampleProject"
         env="rc-www-search"
         zone="us-east1-c"
         delete "gs://mosambi/testout"

      """.stripMargin
    val xml = createActivitWorkFlow(dsl)
    val yaml = createConcourseWorkFlow(dsl)
    yaml.get.nonEmpty shouldBe true
  }

  test("only single line comment") {
    val dsl =
      """name="exampleProject"
         env="rc-www-search"
         zone="us-east1-c"
         // one delete command
         delete "gs://mosambi/testout"
         //another delete comand
         delete "gs://mosambi/testout"
      """.stripMargin
    val xml = createActivitWorkFlow(dsl)
    val yaml = createConcourseWorkFlow(dsl)
    yaml.get.nonEmpty shouldBe true
  }

  test("json add test") {
    val dsl =
      """name="exampleProject"
         env="rc-www-search"
         zone="us-east1-c"
        var a = {"a": "c"}
        var b = {"b":"d"}

         delete a+b
      """.stripMargin

    isValidActivitiWorkflow(dsl)
    isValidConcourseWorkflow(dsl)
  }

  test("create workflow with two actions test") {
    val dsl =
      s"""
                      name="hello"
                      env="stage"
                      zone="east"
                      cluster simpleCluster {
                            workers=2
                            image="simpleimage"
                            properties="example properties"
                            }
                      sparkjob sparkJobName {
                            mainClass="className"
                            args= "sdsd"
                            props="sparkProps"
                            jarLocation="jarURI"
                            }
                      run sparkJobName on  simpleCluster
                      delete "gs://ham/dam/yam"

            """
    isValidActivitiWorkflow(dsl)
    isValidConcourseWorkflow(dsl)
  }

  test("create workflow with nexus jar") {

    val jobName = "nlpRunner"
    val pipelineName = "CreateDataTest"
    val dsl =
      s"""
         name="$pipelineName"
         env="rc-www-search"
         zone="us-east1-c"
         cloudWorkspace="gs://mosambi"

     var mosambi = "gs://mosambi/"
            var output = mosambi + "nlpout/sherindatasetnlp"

    nexus rctechlabReleases {
                 url="https://nexus.rctechlab.com"
                 repository="snapshots"
                 username="{{nexus-user}}"
                 password="{{nexus-password}}"

     }
             sparkjob $jobName {
                 mainClass="com.recipegrace.sa.el.search.nlpapirunner.darshan.NLPRunner"
                 args= "--inputFile",mosambi+"sherindataset.txt","--nlpFile",mosambi+"nlpContent.zip","--output",output,"--partitions","500","--dataset","sherin"
                 jarLocation= % "com.recipegrace.sa.el.burette" % "nlpapirunner_2.11" % "0.0.3-SNAPSHOT" from rctechlabReleases
             }
             cluster simpleCluster {
                 workers=10
                 image="n1-standard-16"
                 properties="spark:spark.executor.cores=16"
             }

         delete output
                  run nlpRunner on  simpleCluster
            """
    val job = getKeyValuePairs(dsl)("jobs")(0)

    job("name").toString shouldBe pipelineName
    val submitTaskConfig= job("plan").asInstanceOf[List[Map[String, Map[String, Any]]]](2)("config")
    val args = submitTaskConfig("run").asInstanceOf[Map[String, Any]]("args").asInstanceOf[List[String]]
    val argMap = args.drop(3).grouped(2).map(f=> f.head-> f(1)).toMap

    argMap("-jarURIs") shouldBe "gs://mosambi/nlpapirunner_2.11-0.0.3-SNAPSHOT.jar"

  }
  test("create workflow reorder test") {
    val dsl =
      s"""
                      name="CreateDataTest"
                      env="rc-www-search"
                      zone="us-east1-c"
                      sparkjob sparkJobName {
                            mainClass="com.recipegrace.biglibrary.electricexamples.CreateData"
                            args= "--output,gs://mosambi/testout"
                            jarLocation="gs://mosambi/ElectricTemplate-0.0.1.jar"
                            }
                      cluster simpleCluster {
                            workers=2
                            image="n1-standard-4"
                            properties="spark:spark.executor.cores=4"
                            }
                      run sparkJobName on  simpleCluster
                      delete "gs://mosambi/testout"

            """
    isValidActivitiWorkflow(dsl)
    isValidConcourseWorkflow(dsl)
  }

  test("re use cluster") {
    val dsl =
      s"""
                      /* This is an example comment */
                      name="CreateDataTest"
                      env="rc-www-search"
                      zone="us-east1-c"
                      sparkjob sparkJobName {
                            mainClass="com.recipegrace.biglibrary.electricexamples.CreateData"
                            args= "--output,gs://mosambi/testout"
                            jarLocation="gs://mosambi/ElectricTemplate-0.0.1.jar"
                            }
                      cluster simpleCluster {
                            workers=2
                            image="n1-standard-4"
                            properties="spark:spark.executor.cores=4"
                            }
                      run sparkJobName on  simpleCluster
                      delete "gs://mosambi/testout"
                      run sparkJobName on  simpleCluster
                      delete "gs://mosambi/testout"

            """


    //isValidActivitiWorkflow(dsl)
    isValidConcourseWorkflow(dsl)
  }


  test("cluster usage on pipeline job") {
    isValidConcourseWorkflow(
      """      name="CreateDataTest"
                     env="rc-www-search"
                       zone="us-east1-c"
       pysparkjob example1(test1, test2) {
         mainPyFile=test1
         otherPyFiles=test2
         }
       pysparkjob example2(test) {
         mainPyFile=test
         otherPyFiles="ssds"
         }
                      cluster simpleCluster {
                            workers=2
                            image="n1-standard-4"
                            properties="spark:spark.executor.cores=4"
                            }

        pipeline letuscheck(two) {
          run example1 (two,"second") on simpleCluster
        }
        run letuscheck("first")
        run example2("second") on simpleCluster
      """.stripMargin)
  }

  test(" cluster with  two properties") {
    val dsl =
      s"""
                      /* This is an example comment */
                      name="CreateDataTest"
                      env="rc-www-search"
                      zone="us-east1-c"
                      sparkjob sparkJobName {
                            mainClass="com.recipegrace.biglibrary.electricexamples.CreateData"
                            args= "--output,gs://mosambi/testout"
                            jarLocation="gs://mosambi/ElectricTemplate-0.0.1.jar"
                            }
                      cluster simpleCluster {
                            workers=2
                            image="n1-standard-4"
                            properties="spark:spark.executor.cores=4","spark:spark.yarn.executor.memoryOverhead=5120"

                            }
                      run sparkJobName on  simpleCluster

            """


    //isValidActivitiWorkflow(dsl)
    isValidConcourseWorkflow(dsl)
  }
}
