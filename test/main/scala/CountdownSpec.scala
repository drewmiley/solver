package main.scala

import org.scalatest._

class CountdownSpec extends FlatSpec with BeforeAndAfterEach {

  var countdown: Countdown = _

  override def beforeEach() {
    countdown = new Countdown(List(), 0)
  }

  "generateIndexPairs" should "be a list of size 1 for param 2" in {
    assert(countdown.generateIndexPairs(2).size === 1)
  }
}