package com.recipegrace.bbc.flowblocks

import com.recipegrace.bbc.BaseTest
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.workflow.DeleteGCSActionFlowBlock

/**
  * Created by Ferosh Jacob on 2/16/17.
  */
class DeleteGCSActionBlockTest extends BaseTest {


  test("display name delete") {
    val example = "one"
    val deleteGCSActionBlock = new DeleteGCSActionFlowBlock(null,StringExpression(s"gs://hello/$example"))

    deleteGCSActionBlock.getLastPart shouldBe example
  }
}
