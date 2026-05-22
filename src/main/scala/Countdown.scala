package main

import scala.annotation.tailrec

object Countdown {

  def generateIndexPairs(listSize: Int): List[(Int, Int)] =
    (0 until listSize).flatMap(i => (i + 1 until listSize).map(j => (i, j))).toList

  private val indexPairsListSize2 = generateIndexPairs(2)
  private val indexPairsListSize3 = generateIndexPairs(3)
  private val indexPairsListSize4 = generateIndexPairs(4)
  private val indexPairsListSize5 = generateIndexPairs(5)
  private val indexPairsListSize6 = generateIndexPairs(6)

  private def getIndexPairs(listSize: Int): List[(Int, Int)] = {
    listSize match {
      case 2 => indexPairsListSize2
      case 3 => indexPairsListSize3
      case 4 => indexPairsListSize4
      case 5 => indexPairsListSize5
      case 6 => indexPairsListSize6
      case _ => generateIndexPairs(listSize)
    }
  }

  def applyOperatorsToIntegerPair(min: Int, max: Int): List[Operation] = {
    List(
      AdditionOperation(min, max),
      SubtractionOperation(min, max),
      MultiplyOperation(min, max),
      DivideOperation(min, max)
    ).filter(operation => operation.value > 0 && operation.value % 1 == 0 &&
      operation.value != min && operation.value != max)
  }

  def operateOnIntegerPairAndCreateNewLists(numberList: Calculation, indexPair: (Int, Int)): List[Calculation] = {
    val min: Int = numberList.values(indexPair._1)
    val max: Int = numberList.values(indexPair._2)
    val listWithIndexPairRemoved: List[Int] = numberList.values diff List(min, max)
    applyOperatorsToIntegerPair(min, max).map(operation =>
      Calculation(
        (listWithIndexPairRemoved :+ operation.value.toInt).sorted,
        numberList.representation :+ operation.representation
      )
    )
  }

  def performOneOperationOnCurrentLists(listOfNumberLists: List[Calculation]): List[Calculation] =
    listOfNumberLists
      .flatMap(numberList =>
        getIndexPairs(numberList.values.size)
          .flatMap(operateOnIntegerPairAndCreateNewLists(numberList, _))
      )

  def filterDuplicateCalculations(calculations: List[Calculation]): List[Calculation] = {
    val calculationsGroupedByValues: Map[List[Int], List[Calculation]] = calculations.groupBy(_.values)
    calculationsGroupedByValues.map(_._2.head).toList
  }

  def filterDuplicateSolutions(solutions: List[Calculation], newSolutions: List[Calculation]): List[Calculation] = {
    val validNewSolutions = newSolutions.filter(newSolution => {
      !solutions.exists(solution => (solution.representation diff newSolution.representation).isEmpty)
    })
    solutions ++ validNewSolutions
  }

  def getNewState(target: Int, filterDuplicate: Boolean)(state: State): State = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val currentCalculationsWithFilteredDuplicate: List[Calculation] =
      if (filterDuplicate) filterDuplicateCalculations(calculatedValues) else calculatedValues
    val calculationsSplitByIfTarget: Map[Boolean, List[Calculation]] =
      currentCalculationsWithFilteredDuplicate.groupBy(_.values.contains(target))
    val currentResult = calculationsSplitByIfTarget.getOrElse(false, List.empty)
    val solutions = filterDuplicateSolutions(state.solutions, calculationsSplitByIfTarget.getOrElse(true, List.empty))
    val newState = State(currentResult, solutions)
    newState
  }

  def solve(picked: List[Int], target: Int, filterDuplicate: Boolean): State = {
    val state: State = State(List(Calculation(picked)))
    val initGetNewState: State => State = getNewState(target, filterDuplicate)
    @tailrec
    def recurse(state: State): State = if (state.currentResult.map(_.values).exists(_.size > 1)) {
      val newState = initGetNewState(state)
      recurse(newState)
    } else {
      state
    }
    recurse(state)
  }
}
