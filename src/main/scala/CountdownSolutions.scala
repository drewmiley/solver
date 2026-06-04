package main

import main.Operations.{filterDuplicateCalculations, performOneOperationOnCurrentLists}

import scala.annotation.tailrec

object CountdownSolutions {

  private def filterDuplicateSolutions(solutions: List[Calculation], newSolutions: List[Calculation]): List[Calculation] = {
    val validNewSolutions = newSolutions.filter(newSolution => {
      !solutions.exists(solution => (solution.representation diff newSolution.representation).isEmpty)
    })
    solutions ++ validNewSolutions
  }

  def getNewState(target: Int, filterDuplicate: Boolean)(state: SolutionsState): SolutionsState = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val currentCalculationsWithFilteredDuplicate: List[Calculation] =
      if (filterDuplicate) filterDuplicateCalculations(calculatedValues) else calculatedValues
    val calculationsSplitByIfTarget: Map[Boolean, List[Calculation]] =
      currentCalculationsWithFilteredDuplicate.groupBy(_.values.contains(target))
    val currentResult = calculationsSplitByIfTarget.getOrElse(false, List.empty)
    val solutions = filterDuplicateSolutions(state.solutions, calculationsSplitByIfTarget.getOrElse(true, List.empty))
    val newState = SolutionsState(currentResult, solutions)
    newState
  }

  def solve(picked: List[Int], target: Int, filterDuplicate: Boolean): SolutionsState = {
    val state: SolutionsState = SolutionsState(List(Calculation(picked)))
    val initGetNewState: SolutionsState => SolutionsState = getNewState(target, filterDuplicate)
    @tailrec
    def recurse(state: SolutionsState): SolutionsState = if (state.currentResult.map(_.values).exists(_.size > 1)) {
      val newState = initGetNewState(state)
      recurse(newState)
    } else {
      state
    }
    recurse(state)
  }
}
