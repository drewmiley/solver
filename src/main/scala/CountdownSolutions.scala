package main

import main.Operations.{filterDuplicateCalculations, performOneOperationOnCurrentLists}

import scala.annotation.tailrec

object CountdownSolutions {

  private def filterDuplicateSolutions(solutions: List[Calculation], newSolutions: List[Calculation]): List[Calculation] = {
    val validNewSolutions = newSolutions.filter(newSolution => {
      !solutions.exists(solution => (solution.representation diff newSolution.representation).isEmpty)
    })
    (solutions ++ validNewSolutions).sortBy(_.values.length).reverse
  }

  def initGetNewState(target: Int, filterDuplicate: Boolean)(state: SolutionsState): SolutionsState = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val calculationsSplitByIfTarget: Map[Boolean, List[Calculation]] =
      calculatedValues.groupBy(_.values.contains(target))
    val calculationsNotTarget = calculationsSplitByIfTarget.getOrElse(false, List.empty)

    val currentResult: List[Calculation] =
      if (filterDuplicate) filterDuplicateCalculations(calculationsNotTarget) else calculationsNotTarget
    val solutions = filterDuplicateSolutions(state.solutions, calculationsSplitByIfTarget.getOrElse(true, List.empty))

    SolutionsState(currentResult, solutions)
  }

  def solveForSolutions(picked: List[Int], target: Int, filterDuplicate: Boolean): SolutionsState = {
    val state: SolutionsState = SolutionsState(List(Calculation(picked)))
    val getNewState: SolutionsState => SolutionsState = initGetNewState(target, filterDuplicate)
    @tailrec
    def recurse(state: SolutionsState): SolutionsState = if (state.currentResultValuesLengthIsOne) {
      state
    } else {
      val newState = getNewState(state)
      recurse(newState)
    }
    recurse(state)
  }
}
