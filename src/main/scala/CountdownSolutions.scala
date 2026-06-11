package main

import main.Operations.{filterDuplicateCalculations, performOneOperationOnCurrentLists}

import scala.annotation.tailrec

object CountdownSolutions {

  private def filterDuplicateSolutions(allSolutions: List[Calculation], newSolutions: List[Calculation]): List[Calculation] = {
    val validNewSolutions = newSolutions.filter(newSolution => {
      !allSolutions.exists(solution => (solution.representation diff newSolution.representation).isEmpty)
    })
    allSolutions ++ validNewSolutions
  }

  def initGetNewState(target: Int, filterDuplicate: Boolean)(state: SolutionsState): SolutionsState = {
    val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
    val calculationsSplitByIfTarget: Map[Boolean, List[Calculation]] =
      calculatedValues.groupBy(_.values.contains(target))
    val calculationsNotTarget = calculationsSplitByIfTarget.getOrElse(false, List.empty)
    val currentResult: List[Calculation] =
      if (filterDuplicate) filterDuplicateCalculations(calculationsNotTarget) else calculationsNotTarget
    val solutions = filterDuplicateSolutions(state.allSolutions, calculationsSplitByIfTarget.getOrElse(true, List.empty))

    SolutionsState(currentResult, solutions)
  }

//  TODO: 2,3,7,9,10,50 target 250
//  TODO: Why is 50 / 2 = 25, 25 * 10 = 250 not a solution??
  def solveForSolutions(picked: List[Int], target: Int, filterDuplicate: Boolean): SolutionsState = {
    val picked = List(2,3,7,9,10,50)
    val target = 250
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
