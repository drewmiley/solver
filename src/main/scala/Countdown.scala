package main

import scala.annotation.tailrec

case class Calculation(values: List[Int], representation: List[String] = List.empty)

case class Operation(value: Float, representation: String)

case class State(currentResult: List[Calculation], solutions: List[Calculation] = List.empty)

object Countdown {

  def applyOperatorsToIntegerPair(min: Int, max: Int): List[Operation] = {
    List(
      Operation(min + max, s"$min + $max = ${ min + max }"),
      Operation(max - min, s"$max - $min = ${ max - min }"),
      Operation(min * max, s"$min * $max = ${ min * max }"),
      Operation(max.toFloat / min, s"$max / $min = ${ max / min }")
    ).filter(operation => operation.value > 0 && operation.value % 1 == 0 &&
      operation.value != min && operation.value != max)
  }

  def generateIndexPairs(listSize: Int): List[(Int, Int)] = {
    (0 until listSize).flatMap(i => (i + 1 until listSize).map(j => (i, j))).toList
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
        generateIndexPairs(numberList.values.size)
          .flatMap(operateOnIntegerPairAndCreateNewLists(numberList, _))
      )

//  TODO: Add tests
  def filterDuplicateCalculations(calculations: List[Calculation]): List[Calculation] = {
    val calculationsGroupedByValues: Map[List[Int], List[Calculation]] = calculations.groupBy(_.values)
    calculationsGroupedByValues.map(_._2.head).toList
  }

//  TODO: Add tests
  def getNewState(target: Int, filterDuplicate: Boolean)(state: State): State = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val currentCalculationsWithFilteredDuplicate: List[Calculation] =
      if (filterDuplicate) filterDuplicateCalculations(calculatedValues) else calculatedValues
    val calculationsSplitByIfTarget: Map[Boolean, List[Calculation]] =
      currentCalculationsWithFilteredDuplicate.groupBy(_.values.contains(target))
    val currentResult = calculationsSplitByIfTarget.getOrElse(false, List.empty)
    val solutions = state.solutions ++ calculationsSplitByIfTarget.getOrElse(true, List.empty)
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
