package main

import main.CountdownNoSolutions.solveForNoSolutions
import main.Util.{getPickedNumbers, printValue}

object Experiment {

  private val largeNumbers: List[Int] = (1 to 4).map(d => 25 * d).toList
  private val smallNumbers: List[Int] = (1 to 10).toList ++ (1 to 10).toList

  private val all0LargePermuations: List[List[Int]] = List.empty
  private val all1LargePermuations: List[List[Int]] = largeNumbers.combinations(1).toList
  private val all2LargePermuations: List[List[Int]] = largeNumbers.combinations(2).toList
  private val all3LargePermuations: List[List[Int]] = largeNumbers.combinations(3).toList
  private val all4LargePermuations: List[List[Int]] = largeNumbers.combinations(4).toList

  private val all6SmallPermuations: List[List[Int]] = smallNumbers.combinations(6).toList
  private val all5SmallPermuations: List[List[Int]] = smallNumbers.combinations(5).toList
  private val all4SmallPermuations: List[List[Int]] = smallNumbers.combinations(4).toList
  private val all3SmallPermuations: List[List[Int]] = smallNumbers.combinations(3).toList
  private val all2SmallPermuations: List[List[Int]] = smallNumbers.combinations(2).toList

  def runExperiment(): Unit = {
    val pickedNumbers = getPickedNumbers(Some(List(1, 2, 3, 4, 6, 20)))

    printValue("Picked", pickedNumbers.mkString(", "))

    val targetRangeLower = 101
    val targetRangeHigher = 999
    val targetRange = targetRangeLower to targetRangeHigher
    printValue("Target Min", targetRangeLower.toString)
    printValue("Target Max", targetRangeHigher.toString)

    val numbersLeftToSolve: List[Int] = solveForNoSolutions(pickedNumbers, targetRange).numbersLeftToSolve
    printValue("No Solutions for", numbersLeftToSolve.mkString(", "))

    //  TODO: Implement
    val all0Large6SmallCombinations: List[List[Int]] = List.empty
    val all1Large5SmallCombinations: List[List[Int]] = List.empty
    val all2Large4SmallCombinations: List[List[Int]] = List.empty
    val all3Large3SmallCombinations: List[List[Int]] = List.empty
    val all4Large2SmallCombinations: List[List[Int]] = List.empty
  }

}
