package main

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

    val solutions: List[Calculation] = findSolutions(picked, smallRandom, largeRandom, target, filterDuplicate)
    val solutionsDisplayText = if (filterDuplicate) "No. Solutions (Distinct)" else "No. Solutions (inc. duplicate)"
    printValue(solutionsDisplayText, solutions.length.toString)
    if (displaySolutions) solutions.map(_.representation.mkString(", ")).foreach(printValue("Solved", _))
  }

  private def findSolutions(picked: Option[List[Int]] = None,
                            smallRandom: Option[Int] = None,
                            largeRandom: Option[Int] = None,
                            target : Option[Int] = None,
                            filterDuplicate: Boolean = true): List[Calculation] = {
    def getPickedNumbers: List[Int] = {
      picked match {
        case Some(intList) => intList
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

    val solutions: List[Calculation] = Countdown.solve(pickedNumbers, targetNumber, filterDuplicate).solutions
    solutions
  }
}
