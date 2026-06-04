package main

import main.Operations.{filterDuplicateCalculations, performOneOperationOnCurrentLists}

import scala.annotation.tailrec

object CountdownNoSolutions {

  def getNewStateSolvingForEveryNumber(state: NoSolutionsState): NoSolutionsState = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val currentCalculationsWithFilteredDuplicate: List[Calculation] = filterDuplicateCalculations(calculatedValues)

    val solvedNumbers: List[Int] = currentCalculationsWithFilteredDuplicate.flatMap(_.values).distinct
    val newNumbersLeftToSolve: List[Int] = state.numbersLeftToSolve diff solvedNumbers

    val newState = NoSolutionsState(currentCalculationsWithFilteredDuplicate, newNumbersLeftToSolve)
    newState
  }

  def solveForEveryNumber(picked: List[Int], targetRange: Range): NoSolutionsState = {
    val state: NoSolutionsState = NoSolutionsState(List(Calculation(picked)), targetRange.toList)
    @tailrec
    def recurse(state: NoSolutionsState): NoSolutionsState = if (state.currentResult.map(_.values).exists(_.size > 1)) {
      val newState = getNewStateSolvingForEveryNumber(state)
      recurse(newState)
    } else {
      state
    }
    recurse(state)
  }
}
