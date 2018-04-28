package main.scala

import org.scalatest._

class CountdownSpec extends FlatSpec with BeforeAndAfterEach {

  var countdown: Countdown = _

  override def beforeEach() {
    countdown = new Countdown(List(), 0)
  }

  def generateSortedIntList(size: Int): List[Int] = {
    new NumberPicker((1 to 100).toList).select(size).sorted
  }

  "applyOperatorsToIntegerPair" should "be Set(2) for param 1, 1" in {
    assert(countdown.applyOperatorsToIntegerPair(1, 1) == Set(2))
  }

  "applyOperatorsToIntegerPair" should "be a set of at most size 3 for param i, i" in {
    (2 to 100).foreach(i => assert(countdown.applyOperatorsToIntegerPair(i, i).size <= 3))
  }

  "applyOperatorsToIntegerPair" should "be a set of at most size 4 for param i, j" in {
    (2 to 100).foreach(i => (i + 1  to 100).foreach(j => assert(countdown.applyOperatorsToIntegerPair(i, j).size <= 4)))
  }

  "applyOperatorsToIntegerPair" should "not return any value <= 0 for param i, j" in {
    (2 to 100).foreach(i => (i + 1 to 100).foreach(j => assert(countdown.applyOperatorsToIntegerPair(i, j).forall(value => value > 0))))
  }

  "applyOperatorsToIntegerPair" should "not return any non-int value for param i, j" in {
    (2 to 100).foreach(i => (i + 1 to 100).foreach(j => assert(countdown.applyOperatorsToIntegerPair(i, j).forall(value => value % 1 == 0))))
  }

  "applyOperatorsToIntegerPair" should "not return i or j for param i, j" in {
    (2 to 100).foreach(i => (i + 1 to 100).foreach(j => assert(countdown.applyOperatorsToIntegerPair(i, j).forall(value => !List(i, j).contains(value)))))
  }

  "generateIndexPairs" should "be List((0, 1)) for param 2" in {
    assert(countdown.generateIndexPairs(2) == List((0, 1)))
  }

  "generateIndexPairs" should "be a list of size T(n - 1) for param n >= 2" in {
    (2 to 100).foreach(n => assert(countdown.generateIndexPairs(n).size == n * (n - 1) / 2))
  }

  "generateIndexPairs" should "be a list of tuples with _2 > _1" in {
    (2 to 100).foreach(n => assert(countdown.generateIndexPairs(n).forall(indexPair => indexPair._1 < indexPair._2)))
  }

  "generateIndexPairs" should "be a list of tuples with 1 <= _2 <= n" in {
    (2 to 100).foreach(n => assert(countdown.generateIndexPairs(n).forall(indexPair => (1 to n).contains(indexPair._2))))
  }

  "generateIndexPairs" should "be a list of tuples with 0 <= _2 <= n + 1" in {
    (2 to 100).foreach(n => assert(countdown.generateIndexPairs(n).forall(indexPair => (0 to n + 1).contains(indexPair._1))))
  }

  "operateOnIntegerPairAndCreateNewLists" should "be a List(List(2)) for params List(1, 1), (0, 1)" in {
    assert(countdown.operateOnIntegerPairAndCreateNewLists(List(1, 1), (0, 1)) == List(List(2)))
  }

  "operateOnIntegerPairAndCreateNewLists" should "be a List of at most size 4" in {
    (2 to 6).foreach(size =>
      (1 to 100).foreach(_ =>
        assert(countdown.operateOnIntegerPairAndCreateNewLists(generateSortedIntList(size), (0, 1)).size <= 4)))
  }

  "operateOnIntegerPairAndCreateNewLists" should "be a List of lists with size 1 less than the param list" in {
    (2 to 6).foreach(size =>
      (1 to 100).foreach(_ =>
        assert(countdown.operateOnIntegerPairAndCreateNewLists(generateSortedIntList(size), (0, 1))
          .map(list => list.size).toSet == Set(size - 1))))
  }

  "performOneOperationOnCurrentLists" should "be a List(List(2)) for param List(List(1, 1))" in {
    assert(countdown.performOneOperationOnCurrentLists(List(List(1, 1))) == List(List(2)))
  }

  "performOneOperationOnCurrentLists" should "bbe a List of at most 4 times the size of the param lists" in {
  }

  "performOneOperationOnCurrentLists" should "be a List of lists with size 1 less than the param lists" in {
  }
}