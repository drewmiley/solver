package main

import main.Operations.{filterDuplicateCalculations, performOneOperationOnCurrentLists}

import scala.annotation.tailrec

object CountdownNoSolutions {

  def getNewState(state: NoSolutionsState): NoSolutionsState = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val currentCalculationsWithFilteredDuplicate: List[Calculation] = filterDuplicateCalculations(calculatedValues)

    val solvedNumbers: List[Int] = currentCalculationsWithFilteredDuplicate.flatMap(_.values).distinct
    val newNumbersLeftToSolve: List[Int] = state.numbersLeftToSolve diff solvedNumbers

    NoSolutionsState(currentCalculationsWithFilteredDuplicate, newNumbersLeftToSolve)
  }

  def solveForNoSolutions(picked: List[Int], targetRange: Range): NoSolutionsState = {
    val state: NoSolutionsState = NoSolutionsState(List(Calculation(picked)), targetRange.toList)
    @tailrec
    def recurse(state: NoSolutionsState): NoSolutionsState = if (state.currentResultValuesLengthIsOne) {
      state
    } else {
      val newState = getNewState(state)
      recurse(newState)
    }
    recurse(state)
  }
}
