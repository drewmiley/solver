package main.scala

import org.scalatest._

class CountdownSpec extends FlatSpec with BeforeAndAfterEach {

  var countdown: Countdown = _

  override def beforeEach() {
    countdown = new Countdown(List(), 0)
  }

  "applyOperatorsToIntegerPair" should "be a set of size 1 for param 1, 1" in {
    assert(countdown.applyOperatorsToIntegerPair(1, 1).size == 1)
  }

  "generateIndexPairs" should "be a list of size 1 for param 2" in {
    assert(countdown.generateIndexPairs(2).size == 1)
  }

  "operateOnIntegerPairAndCreateNewLists" should "be a list of size 1 for params List(1, 1), (0, 1)" in {
    assert(countdown.operateOnIntegerPairAndCreateNewLists(List(1, 1), (0, 1)).size == 1)
  }

  "performOneOperationOnCurrentLists" should "be a list of size 1 for param List(List(1, 1))" in {
    assert(countdown.performOneOperationOnCurrentLists(List(List(1, 1))).size == 1)
  }
}