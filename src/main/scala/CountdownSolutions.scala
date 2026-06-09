package main

import main.Operations.{filterDuplicateCalculations, performOneOperationOnCurrentLists}

import scala.annotation.tailrec

object CountdownSolutions {

  private def filterDuplicateSolutions(solutions: List[Calculation], newSolutions: List[Calculation]): List[Calculation] = {
//    TODO: Look in here for why??
    val validNewSolutions = newSolutions.filter(newSolution => {
      !solutions.exists(solution => (solution.representation diff newSolution.representation).isEmpty)
    })
    solutions ++ validNewSolutions
  }

  def initGetNewState(target: Int, filterDuplicate: Boolean)(state: SolutionsState): SolutionsState = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val currentCalculationsWithFilteredDuplicate: List[Calculation] =
      if (filterDuplicate) filterDuplicateCalculations(calculatedValues) else calculatedValues

    val calculationsSplitByIfTarget: Map[Boolean, List[Calculation]] =
      currentCalculationsWithFilteredDuplicate.groupBy(_.values.contains(target))
    val currentResult = calculationsSplitByIfTarget.getOrElse(false, List.empty)
    val solutions = filterDuplicateSolutions(state.solutions, calculationsSplitByIfTarget.getOrElse(true, List.empty))

    SolutionsState(currentResult, solutions)
  }

//  TODO: 2,3,7,9,10,50 target 250
//  TODO: Why is 50 / 2 = 25, 25 * 10 = 250 not a solution??
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
