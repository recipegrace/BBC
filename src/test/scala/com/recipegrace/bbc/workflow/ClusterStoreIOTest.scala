package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.BaseTest
import com.recipegrace.bbc.grmr.BBCStructures.ClusterStore

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class ClusterStoreIOTest extends BaseTest with ClusterStoreIO {

  test("cluster adding and delete test") {
    val clusterStore = new ClusterStore(List())
    val one = "one"
    val oneClusterStore = addToClusterStore(one, clusterStore)
    oneClusterStore.running should have size 1
    isClusterRunning(oneClusterStore, one) shouldBe true
    removeFromClusterStore(one, oneClusterStore).running should have size 0
  }
}
