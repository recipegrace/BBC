package com.recipegrace.bbc.composer

import com.recipegrace.bbc.BaseTest
import com.recipegrace.bbc.codegen.ExpressionCreator
import com.recipegrace.bbc.workflow.ProgramConfiguration
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.BaseBBCGrammarTest
import com.recipegrace.bbc.grmr.Expressions.Expression



class TemplateTest extends BaseBBCGrammarTest with ExpressionCreator{

  val taskName = "name"

  test("python job "){
    val programConfiguration = ProgramConfiguration(expr("one"), expr("zone"), "name", None)
    val localVariables = Map(): Map[String, Expression]
    object evalObject extends ExpressionCreator
    val mainPyFile = "hola"
    val props = "-x.ua  y.ub"
    val programArguments = Array("arg1", "arg2")
    val bucketName = "something"
    val objectName = "somethingelse"
    val jarLocation = s"gs://${bucketName}/${objectName}"
    val jobName = "Zjob"


    val jobDefintiion =
      s"""pythonjob $jobName {
        mainPyFile="$mainPyFile"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        }
      """.stripMargin


    val job = parseAll(_pyJobBody, jobDefintiion).get


    Templates.translate("templates/run-python.ssp", Map("name" -> taskName, "localVariables" -> localVariables,
      "evalObject" -> evalObject,
      "programConfiguration" -> programConfiguration,
      "pythonJob" -> job._2)) shouldBe "\n" +
      s"""  from $mainPyFile import main
         |  $taskName = python_operator.PythonOperator(
         |   task_id='run_python_${taskName}',
         |   op_args=[${programArguments.map(f => "'" + f + "'").mkString(",")}],
         |   python_callable=main
  )""".stripMargin
  }

  test("java job "){
    val programConfiguration = ProgramConfiguration(expr("one"), expr("zone"), "name", None)
    val localVariables = Map(): Map[String, Expression]
    object evalObject extends ExpressionCreator
    val mainClass = "hola"
    val props = "-x.ua  y.ub"
    val programArguments = Array("arg1", "arg2")
    val bucketName = "something"
    val objectName = "somethingelse"
    val jarLocation = s"gs://${bucketName}/${objectName}"
    val jobName = "Zjob"
    val jobDefintiion =
      s"""javajob $jobName {
        mainClass="$mainClass"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        props="$props"
        jarLocation="$jarLocation"
        }
      """.stripMargin


    val job = parseAll(_javaJobBody, jobDefintiion).get

    /*
    Templates.translate("templates/download-jar.ssp", Map("name" -> taskName, "localVariables" -> localVariables,
      "evalObject" -> evalObject,
      "programConfiguration" -> programConfiguration,
      "javaJob" -> job._2)) shouldBe "\n" +
      s"""  $taskName = gcs_download_operator.GoogleCloudStorageDownloadOperator(
         |   task_id='copy_jar_${taskName}',
         |   bucket='${bucketName}',
         |   object='${objectName}',
         |   filename='/tmp/${objectName}'
  )""".stripMargin
  */

    Templates.translate("templates/run-jar.ssp", Map("name" -> taskName, "localVariables" -> localVariables,
      "evalObject" -> evalObject,
      "programConfiguration" -> programConfiguration,
      "javaJob" -> job._2)) shouldBe "\n" +
      s"""  $taskName = bash_operator.BashOperator(
         |   task_id='run_jar_${taskName}',
         |   bash_command='gsutil -m cp -r gs://${bucketName}/${objectName} .;java -cp ${objectName} $mainClass ${programArguments.mkString(" ")}'
  )""".stripMargin
  }
  test("delete gcs") {
    val localVariables = Map(): Map[String, Expression]
    object evalObject extends ExpressionCreator
    val location = "gs://hola/mola"

    Templates.translate("templates/delete-gcs.ssp",Map("name" -> taskName, "localVariables" -> localVariables,
      "evalObject" -> evalObject, "location" -> expr(location))) shouldBe """  bucket_name, object_name = 'gs://hola/mola'[5:].split('/', 1)
                                                                                |  name = airflow.contrib.operators.GoogleCloudStorageListOperator(
                                                                                |    task_id='name',
                                                                                |    # Give the cluster a unique name by appending the date scheduled.
                                                                                |    # See https://airflow.apache.org/code.html#default-variables
                                                                                |    bucket=bucket_name,
                                                                                |    prefix=object_name
                                                                                |  )
                                                                                |""".stripMargin

  }
  test("template test for submit pyspark job") {
    val programConfiguration = ProgramConfiguration(expr("one"), expr("zone"), "name", None)
    val localVariables = Map(): Map[String, Expression]
    object evalObject extends ExpressionCreator
    val mainPyFile = "samplel.py"
    val sparkProps = "ua==BX,ub==AX"
    val programArguments = Array("arg1", "arg2")
    val otherPyFiles = Array("sample1.py","sample2.py")
    val jobName = "Zjob"
    val jobDefintiion =
      s"""pysparkjob $jobName {
        mainPyFile="$mainPyFile"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        props="$sparkProps"
        otherPyFiles=${otherPyFiles.map(f => "\"" + f + "\"").mkString(",")}
        }
      """.stripMargin


    val job = parseAll(_pySparkJobBody, jobDefintiion).get

    val clusterName = "cluster1"

    Templates.translate("templates/run-pyspark.ssp", Map("name" -> taskName, "localVariables" -> localVariables,
      "evalObject" -> evalObject,
      "programConfiguration" -> programConfiguration,
      "sparkJob" -> job._2,
      "clusterName" -> clusterName)) shouldBe "\n" +
      s"""  $taskName = dataproc_operator.DataProcPySparkOperator(
         |    task_id='submit_spark_${taskName}',
         |    main='${mainPyFile}',
         |    job_name='submit_spark_${job._2.name}',
         |    pyfiles=[${otherPyFiles.map(f => "'" + f + "'").mkString(",")}],
         |    arguments=[${programArguments.map(f => "'" + f + "'").mkString(",")}],
         |    dataproc_spark_properties={'ua':'BX', 'ub':'AX'},
         |    cluster_name='${clusterName}'.lower()
  )""".stripMargin
  }

  test("template test for submit spark job") {
    val programConfiguration = ProgramConfiguration(expr("one"),expr("zone"),"name",None)
    val localVariables = Map():Map[String, Expression]
    object evalObject extends ExpressionCreator
    val className = "com.x.y.Z"
    val sparkProps = "a==b,b==c"
    val programArguments = Array("arg1", "arg2")
    val jarURI = "jar link"
    val jobName = "Zjob"


    val jobDefintiion =
      s"""sparkjob $jobName {
        mainClass="$className"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        props="$sparkProps"
        jarLocation="$jarURI"
        }
      """.stripMargin

    val clusterName = "cluster1"
    val job = parseAll(_sparkJobBody, jobDefintiion).get
    Templates.translate("templates/run-spark.ssp",Map( "name"-> taskName, "localVariables" -> localVariables,
      "evalObject" -> evalObject,
      "programConfiguration" -> programConfiguration,
      "sparkJob" -> job._2 ,
      "clusterName" -> clusterName)) shouldBe "\n"+s"""  $taskName = dataproc_operator.DataProcSparkOperator(
                                                 |    task_id='submit_spark_${taskName}',
                                                 |    dataproc_spark_jars=['${jarURI}'],
                                                 |    main_class='${job._2.sparkJob.mainClass}',
                                                 |    job_name='submit_spark_${job._2.name}',
                                                 |    arguments=[${programArguments.map(f=>"'"+f+"'").mkString(",")}],
                                                 |    dataproc_spark_properties={'a':'b', 'b':'c'},
                                                 |    cluster_name='${clusterName}'.lower()
  )""".stripMargin




  }

  test("template test for  create cluster 2") {
    val subNetworkName = "subNetwork1"
    val timeOut = "40s"
    val script = "gs://hola"
    val props = "someproperties"
    val image = "somageimage"
    val workers = 10
    val tags = Array("tag1", "tag2")
    val jobDefintiion =
      s"""cluster prodClusterSearchRelevance {
         |workers=$workers
         |image="$image"
         |tags= ${tags.map(f => "\"" + f + "\"").mkString(",")}
         |properties="$props"
         |initialScript = "$script"
         |initialScriptTimeOut = "$timeOut"
         |subNetwork = "$subNetworkName"
         |}
      """.stripMargin

    val job = parseAll(_clusterBody, jobDefintiion).get
    val cluster = job._2
    val zone = "zone"
    val programConfiguration = ProgramConfiguration(expr("one"),expr(s"$zone"),"name",None)
    val localVariables = Map():Map[String, Expression]
    object evalObject extends ExpressionCreator
    val message =Templates.translate("templates/create-cluster.ssp",Map("name"-> taskName,
      "localVariables" -> localVariables,
      "evalObject" -> evalObject,
      "programConfiguration" -> programConfiguration,
      "cluster" -> cluster )) shouldBe s"""
         |  name = DataprocClusterCreateOperatorModified(
         |    task_id='create_cluster_$taskName',
         |    # Give the cluster a unique name by appending the date scheduled.
         |    # See https://airflow.apache.org/code.html#default-variables
         |    cluster_name='${cluster.name}'.lower(),
         |    num_workers=$workers,
         |    zone='$zone',
         |    subnetwork_uri='$subNetworkName',
         |    init_actions_uris=['$script'],
         |    init_action_timeout='$timeOut',
         |    tags=[${tags.map(f=>"'"+f+"'").mkString(",")}],
         |    master_machine_type='$image',
         |    worker_machine_type='$image'
         |  )""".stripMargin


  }


  test("template test for  create cluster") {
    val cluster = Cluster(1,"big-bricks",List(ClusterConfigImage(expr("image")),ClusterConfigWorkers(expr("2"))))
    val programConfiguration = ProgramConfiguration(expr("one"),expr("zone"),"name",None)
    val localVariables = Map():Map[String, Expression]
    object evalObject extends ExpressionCreator
    Templates.translate("templates/create-cluster.ssp",Map("name"-> taskName,
      "localVariables" -> localVariables,
      "evalObject" -> evalObject,
      "programConfiguration" -> programConfiguration,
      "cluster" -> cluster )) shouldBe "\n"+s"""  ${taskName} = DataprocClusterCreateOperatorModified(
          |    task_id='create_cluster_$taskName',
          |    # Give the cluster a unique name by appending the date scheduled.
          |    # See https://airflow.apache.org/code.html#default-variables
          |    cluster_name='${cluster.name}'.lower(),
          |    num_workers=${ evalObject.evaluateVariable(cluster.cluster.workers,localVariables)},
          |    zone='${evalObject.evaluateVariable(programConfiguration.zone,localVariables )}',
          |    master_machine_type='${evalObject.evaluateVariable(cluster.cluster.image,localVariables)}',
          |    worker_machine_type='${evalObject.evaluateVariable(cluster.cluster.image,localVariables)}'
          |  )""".stripMargin


  }




  test("templates test") {


    val output = Templates.translate("templates/test.ssp", Map("name" -> ("Hiram", "Chirino"), "city" -> "Tampa"))
    output shouldBe "<p> Hello Hiram Chirino, from Tampa. </p>"
  }
  test("template header test") {
    ComposerFlowBlockCombiner.generateHeaderContent(ProgramConfiguration(expr("env"),expr("zone"),"hola",None)) should have size 18145

  }
}
