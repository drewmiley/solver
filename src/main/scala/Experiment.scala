package main

import main.CountdownNoSolutions.solveForNoSolutions
import main.Util.{getPickedNumbers, printValue}

object Experiment {

  def runExperiment(): Unit = {
    val pickedNumbers = getPickedNumbers(Some(List(1, 2, 3, 4, 6, 20)))

    val targetMin: Option[Int] = Some(100)
    val targetMax: Option[Int] = Some(999)

    printValue("Picked", pickedNumbers.mkString(", "))

    val targetRangeLower = targetMin.getOrElse(101)
    val targetRangeHigher = targetMax.getOrElse(999)
    val targetRange = targetRangeLower to targetRangeHigher
    printValue("Target Min", targetRangeLower.toString)
    printValue("Target Max", targetRangeHigher.toString)

    val numbersLeftToSolve: List[Int] = solveForNoSolutions(pickedNumbers, targetRange).numbersLeftToSolve
    printValue("No Solutions for", numbersLeftToSolve.mkString(", "))
  }

}
