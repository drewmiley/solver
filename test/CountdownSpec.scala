package main

import CountdownSolutions.initGetNewState
import Operations._
import org.scalatest._

class CountdownSpec extends FlatSpec with BeforeAndAfterEach {

  def generateSortedIntList(size: Int): List[Int] = {
    new NumberPicker((1 to 100).toList).select(size).sorted
  }

  "applyOperatorsToIntegerPair" should "be List(Operation(2,1 + 1 = 2)) for param 1, 1" in {
    assert(applyOperatorsToIntegerPair(1, 1) == List(AdditionOperation(1, 1)))
  }

  "applyOperatorsToIntegerPair" should "be a set of at most size 3 for param i, i" in {
    (2 to 100).foreach(i => assert(applyOperatorsToIntegerPair(i, i).size <= 3))
  }

  "applyOperatorsToIntegerPair" should "be a set of at most size 4 for param i, j" in {
    (2 to 100).foreach(i => (i + 1  to 100).foreach(j => assert(applyOperatorsToIntegerPair(i, j).size <= 4)))
  }

  "applyOperatorsToIntegerPair" should "not return any value <= 0 for param i, j" in {
    (2 to 100).foreach(i => (i + 1 to 100).foreach(j => assert(applyOperatorsToIntegerPair(i, j).forall(_.value > 0))))
  }

  "applyOperatorsToIntegerPair" should "not return any non-int value for param i, j" in {
    (2 to 100).foreach(i => (i + 1 to 100).foreach(j => assert(applyOperatorsToIntegerPair(i, j).forall(_.value % 1 == 0))))
  }

  "applyOperatorsToIntegerPair" should "not return i or j for param i, j" in {
    (2 to 100).foreach(i => (i + 1 to 100).foreach(j => assert(applyOperatorsToIntegerPair(i, j).forall(operation => !List(i, j).contains(operation.value)))))
  }

  "generateIndexPairs" should "be List((0, 1)) for param 2" in {
    assert(generateIndexPairs(2) == List((0, 1)))
  }

  "generateIndexPairs" should "be a list of size T(n - 1) for param n >= 2" in {
    (2 to 100).foreach(n => assert(generateIndexPairs(n).size == n * (n - 1) / 2))
  }

  "generateIndexPairs" should "be a list of tuples with _2 > _1" in {
    (2 to 100).foreach(n => assert(generateIndexPairs(n).forall(indexPair => indexPair._1 < indexPair._2)))
  }

  "generateIndexPairs" should "be a list of tuples with 1 <= _2 <= n" in {
    (2 to 100).foreach(n => assert(generateIndexPairs(n).forall(indexPair => (1 to n).contains(indexPair._2))))
  }

  "generateIndexPairs" should "be a list of tuples with 0 <= _2 <= n + 1" in {
    (2 to 100).foreach(n => assert(generateIndexPairs(n).forall(indexPair => (0 to n + 1).contains(indexPair._1))))
  }

  "operateOnIntegerPairAndCreateNewLists" should "be a List(Calculation(List(2),List(1 + 1 = 2))) for params List(1, 1), (0, 1)" in {
    assert(operateOnIntegerPairAndCreateNewLists(Calculation(List(1, 1)), (0, 1)) == List(Calculation(List(2),List("1 + 1 = 2"))))
  }

  "operateOnIntegerPairAndCreateNewLists" should "be a List of at most size 4" in {
    (2 to 6).foreach(size =>
      (1 to 100).foreach(_ =>
        assert(operateOnIntegerPairAndCreateNewLists(Calculation(generateSortedIntList(size)), (0, 1)).size <= 4)))
  }

  "operateOnIntegerPairAndCreateNewLists" should "be a List of lists with size 1 less than the param list" in {
    (2 to 6).foreach(size =>
      (1 to 100).foreach(_ =>
        assert(operateOnIntegerPairAndCreateNewLists(Calculation(generateSortedIntList(size)), (0, 1))
          .map(list => list.values.size).toSet == Set(size - 1))))
  }

  "performOneOperationOnCurrentLists" should "be a List(Calculation(List(2),List(1 + 1 = 2))) for param List(List(1, 1))" in {
    assert(performOneOperationOnCurrentLists(List(Calculation(List(1, 1)))) == List(Calculation(List(2),List("1 + 1 = 2"))))
  }

  "performOneOperationOnCurrentLists" should "be a List of at most 4 * T(size - 1) times the size of the param lists" in {
    (2 to 6).foreach(size =>
      (1 to 100).foreach(_ => {
        val generatedList = (1 to 100).toList.map(_ => generateSortedIntList(size))
        assert(performOneOperationOnCurrentLists(generatedList.map(Calculation(_))).size <= 4 * generatedList.size * (size * (size - 1) / 2))
      }))
  }

  "performOneOperationOnCurrentLists" should "be a List of lists with size 1 less than the param lists" in {
    (2 to 6).foreach(size =>
      (1 to 100).foreach(_ => {
        val generatedList = (1 to 100).toList.map(_ => generateSortedIntList(size))
        assert(performOneOperationOnCurrentLists(generatedList.map(Calculation(_)))
          .map(list => list.values.size).toSet == Set(size - 1))
      }))
  }

  "filterDuplicateCalculations" should "filter to one Calculation per distinct set of values" in {
    (2 to 6).foreach(size =>
      (1 to 100).foreach(_ => {
        val generatedList = (1 to 100).toList.map(_ => generateSortedIntList(size))
        val target = new NumberPicker((1 to 100).toList).select(1).head
        generatedList.foreach(picked => {
          val state: SolutionsState = SolutionsState(List(Calculation(picked)))
          val newState = initGetNewState(target, filterDuplicate = false)(state)
          val newCalculations = newState.currentResult
          assert(filterDuplicateCalculations(newCalculations).length == newCalculations.groupBy(_.values).values.flatten.toList.length)
        })
      }))
  }

  "CountdownSolutions.getNewState" should "return a State where for values in the list, the size is 1 less than for the State passed in" in {
    (2 to 6).foreach(size =>
      (1 to 2).foreach(_ => {
        val generatedList = (1 to 100).toList.map(_ => generateSortedIntList(size))
        val target = new NumberPicker((1 to 100).toList).select(1).head
        val getNewState: SolutionsState => SolutionsState = CountdownSolutions.initGetNewState(target, filterDuplicate = true)
        generatedList.foreach(picked => {
          val initState: SolutionsState = SolutionsState(List(Calculation(picked)))
          var state: SolutionsState = initState
          (1 until size).foreach(stepNo => {
            val newState: SolutionsState = getNewState(state)
            val stateValuesLength = state.currentResult.map(_.values.length).toSet
            val newStateValuesLength = newState.currentResult.map(_.values.length).toSet
            assert(stateValuesLength.size == 1)
            assert(newStateValuesLength.size == 1)
            assert(stateValuesLength.head == size + 1 - stepNo)
            assert(newStateValuesLength.head + 1 == size + 1 - stepNo)
            state = newState
          })
        })
      }))
  }

  "CountdownSolutions.getNewState" should "return a State where the size of solutions is greater than or equal to for the State passed in" in {
    (2 to 6).foreach(size =>
      (1 to 2).foreach(_ => {
        val generatedList = (1 to 100).toList.map(_ => generateSortedIntList(size))
        val target = new NumberPicker((1 to 100).toList).select(1).head
        val getNewState: SolutionsState => SolutionsState = CountdownSolutions.initGetNewState(target, filterDuplicate = true)
        generatedList.foreach(picked => {
          val initState: SolutionsState = SolutionsState(List(Calculation(picked)))
          var state: SolutionsState = initState
          (1 until size).foreach(stepNo => {
            val newState: SolutionsState = getNewState(state)
            assert(newState.allSolutions.length >= state.allSolutions.length)
            state = newState
          })
        })
      }))
  }

  "CountdownNoSolutions.getNewState" should "return a State where for values in the list, the size is 1 less than for the State passed in" in {
    (2 to 6).foreach(size =>
      (1 to 2).foreach(_ => {
        val generatedList = (1 to 100).toList.map(_ => generateSortedIntList(size))
        val targetRange = 101 to 999
        generatedList.foreach(picked => {
          val initState: NoSolutionsState = NoSolutionsState(List(Calculation(picked)), targetRange.toList)
          var state: NoSolutionsState = initState
          (1 until size).foreach(stepNo => {
            val newState: NoSolutionsState = CountdownNoSolutions.getNewState(state)
            val stateValuesLength = state.currentResult.map(_.values.length).toSet
            val newStateValuesLength = newState.currentResult.map(_.values.length).toSet
            assert(stateValuesLength.size == 1)
            assert(newStateValuesLength.size == 1)
            assert(stateValuesLength.head == size + 1 - stepNo)
            assert(newStateValuesLength.head + 1 == size + 1 - stepNo)
            state = newState
          })
        })
      }))
  }

  "CountdownNoSolutions.getNewState" should "return a State where the size of numbersLeftToSolve is less than or equal to for the State passed in" in {
    (2 to 6).foreach(size =>
      (1 to 2).foreach(_ => {
        val generatedList = (1 to 100).toList.map(_ => generateSortedIntList(size))
        val targetRange = 101 to 999
        generatedList.foreach(picked => {
          val initState: NoSolutionsState = NoSolutionsState(List(Calculation(picked)), targetRange.toList)
          var state: NoSolutionsState = initState
          (1 until size).foreach(stepNo => {
            val newState: NoSolutionsState = CountdownNoSolutions.getNewState(state)
            assert(newState.numbersLeftToSolve.length <= state.numbersLeftToSolve.length)
            state = newState
          })
        })
      }))
  }
}