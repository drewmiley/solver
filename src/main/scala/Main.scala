package main

import Countdown._
import Util._

object Main {

  def main(args: Array[String]): Unit = {
    val argsList = args.toList

    val picked: Option[List[Int]] = getConfigIntListFromArgs(argsList, "picked")
    val smallRandom: Option[Int] = getConfigIntFromArgs(argsList, "smallRandom")
    val largeRandom: Option[Int] = getConfigIntFromArgs(argsList, "largeRandom")
    val target : Option[Int] = getConfigIntFromArgs(argsList, "target")
    val filterDuplicate: Boolean = getConfigBoolFromArgs(argsList, "filterDuplicate").getOrElse(true)
    val displaySolutions: Boolean = getConfigBoolFromArgs(argsList, "displaySolutions").getOrElse(true)
    if (sys.env.get("DEMO").contains("true")) runDemo() else {
      val solutions: List[Calculation] = findSolutions(picked, smallRandom, largeRandom, target, filterDuplicate)
      val solutionsDisplayText = if (filterDuplicate) "No. Solutions (Distinct)" else "No. Solutions (inc. duplicate)"
      printValue(solutionsDisplayText, solutions.length.toString)
      if (displaySolutions) solutions.map(_.representation.mkString(", ")).foreach(printValue("Solved", _))
    }
  }

  private def runDemo(): Unit = {
    val pickedNumbers = List(1, 2, 3, 4, 6, 20)
    val targetNumber = 179

    val initGetNewState: State => State = getNewState(targetNumber, filterDuplicate = true)

    val stateStep0: State = State(List(Calculation(pickedNumbers)))
    // valuesLengthStep0 should be Set(6)
    val valuesLengthStep0: Set[Int] = stateStep0.currentResult.map(_.values).map(_.size).toSet
    println("STEP 0 DONE")

    val stateStep1: State = initGetNewState(stateStep0)
    // valuesLengthStep1 should be Set(5)
    val valuesLengthStep1: Set[Int] = stateStep1.currentResult.map(_.values).map(_.size).toSet
    println("STEP 1 DONE")

    val stateStep2: State = initGetNewState(stateStep1)
    // valuesLengthStep2 should be Set(4)
    val valuesLengthStep2: Set[Int] = stateStep2.currentResult.map(_.values).map(_.size).toSet
    println("STEP 2 DONE")

    val stateStep3: State = initGetNewState(stateStep2)
    // valuesLengthStep3 should be Set(3)
    val valuesLengthStep3: Set[Int] = stateStep3.currentResult.map(_.values).map(_.size).toSet
    println("STEP 3 DONE")

    val stateStep4: State = initGetNewState(stateStep3)
    // valuesLengthStep4 should be Set(2)
    val valuesLengthStep4: Set[Int] = stateStep4.currentResult.map(_.values).map(_.size).toSet
    println("STEP 4 DONE")

    val stateStep5: State = initGetNewState(stateStep4)
    // valuesLengthStep5 should be Set(1)
    val valuesLengthStep5: Set[Int] = stateStep5.currentResult.map(_.values).map(_.size).toSet
    println("STEP 5 DONE")

    val solutions = stateStep5.solutions
    println("SOLUTIONS FOUND")
  }

  private def findSolutions(picked: Option[List[Int]] = None,
                            smallRandom: Option[Int] = None,
                            largeRandom: Option[Int] = None,
                            target : Option[Int] = None,
                            filterDuplicate: Boolean = true): List[Calculation] = {
    def getPickedNumbers: List[Int] = {
      picked match {
        case Some(intList) => intList.sorted
        case None =>
          val largeNumbers = largeRandom.getOrElse(1)
          val smallNumbers = smallRandom.getOrElse(6 - largeNumbers)

          val largePicker = new NumberPicker((1 to 4).map(d => 25 * d).toList)
          val smallPicker = new NumberPicker((1 to 10).toList ++ (1 to 10).toList)

          val pickedNumbers = smallPicker.select(smallNumbers).sorted ++ largePicker.select(largeNumbers)
          pickedNumbers
      }
    }

    def getTargetNumber: Int = {
      target match {
        case Some(value) => value
        case None =>
          val answerPicker = new NumberPicker((101 to 999).toList)
          val targetNumber = answerPicker.select(1).head
          targetNumber
      }
    }

    val pickedNumbers = getPickedNumbers
    printValue("Picked", pickedNumbers.mkString(", "))

    val targetNumber = getTargetNumber
    printValue("Target", targetNumber.toString)

    val solutions: List[Calculation] = solve(pickedNumbers, targetNumber, filterDuplicate).solutions
    solutions
  }
}
