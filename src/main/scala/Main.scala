package main

import CountdownSolutions.solveForSolutions
import CountdownNoSolutions.solveForNoSolutions
import Demo.runDemo
import Experiment.runExperiment
import Util._

object Main {

  def main(args: Array[String]): Unit = {
    val argsList = args.toList

    (sys.env.get("DEMO").contains("true"), sys.env.get("EXPERIMENT").contains("true")) match {
      case (true, _) => runDemo()
      case (_, true) => runExperiment()
      case (false, false) => run(argsList)
    }
  }

  private def run(argsList: List[String]): Unit = {
    val picked: Option[List[Int]] = getConfigIntListFromArgs(argsList, "picked")
    val smallRandom: Option[Int] = getConfigIntFromArgs(argsList, "smallRandom")
    val largeRandom: Option[Int] = getConfigIntFromArgs(argsList, "largeRandom")

    getConfigBoolFromArgs(argsList, "noSolutions") match {
      case Some(true) =>
        val pickedNumbers = getPickedNumbers(picked, smallRandom, largeRandom)
        val targetMin: Option[Int] = getConfigIntFromArgs(argsList, "targetMin")
        val targetMax: Option[Int] = getConfigIntFromArgs(argsList, "targetMax")
        val noSolutionsFor: List[Int] = findNoSolutions(pickedNumbers, targetMin, targetMax)
        printValue("No Solutions for", noSolutionsFor.mkString(", "))
      case _ =>
        val target : Option[Int] = getConfigIntFromArgs(argsList, "target")
        val filterDuplicate: Boolean = getConfigBoolFromArgs(argsList, "filterDuplicate").getOrElse(true)
        val displaySolutions: Boolean = getConfigBoolFromArgs(argsList, "displaySolutions").getOrElse(true)
        val pickedNumbers = getPickedNumbers(picked, smallRandom, largeRandom)
        val solutions: List[Calculation] = findSolutions(pickedNumbers, target, filterDuplicate)
        val solutionsDisplayText = if (filterDuplicate) "No. Solutions (Distinct)" else "No. Solutions (inc. duplicate)"
        printValue(solutionsDisplayText, solutions.length.toString)
        if (displaySolutions) solutions.map(_.representation.mkString(", ")).foreach(printValue("Solved", _))
    }
  }

  private def findNoSolutions(pickedNumbers: List[Int],
                              targetMin: Option[Int] = None,
                              targetMax: Option[Int] = None): List[Int] = {
    printValue("Picked", pickedNumbers.mkString(", "))

    val targetRangeLower = targetMin.getOrElse(101)
    val targetRangeHigher = targetMax.getOrElse(999)
    val targetRange = targetRangeLower to targetRangeHigher
    printValue("Target Min", targetRangeLower.toString)
    printValue("Target Max", targetRangeHigher.toString)

    val numbersLeftToSolve: List[Int] = solveForNoSolutions(pickedNumbers, targetRange).numbersLeftToSolve
    numbersLeftToSolve
  }

  private def findSolutions(pickedNumbers: List[Int],
                            target : Option[Int] = None,
                            filterDuplicate: Boolean = true): List[Calculation] = {
    def getTargetNumber: Int = {
      target match {
        case Some(value) => value
        case None =>
          val answerPicker = new NumberPicker((101 to 999).toList)
          val targetNumber = answerPicker.select(1).head
          targetNumber
      }
    }

    printValue("Picked", pickedNumbers.mkString(", "))

    val targetNumber = getTargetNumber
    printValue("Target", targetNumber.toString)

    val solutions: List[Calculation] = solveForSolutions(pickedNumbers, targetNumber, filterDuplicate).solutions
    solutions
  }
}
