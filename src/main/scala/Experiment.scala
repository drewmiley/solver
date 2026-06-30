package main

import main.CountdownNoSolutions.solveForNoSolutions
import main.Util.{getPickedNumbers, printValue}

import scala.util.Random

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

//  TODO: Swap large and small and sorted not required - test this
//  private val all0Large6SmallCombinations: List[List[Int]] = all6SmallPermuations.map(_.sorted)
//  private val all1Large5SmallCombinations: List[List[Int]] = all5SmallPermuations.flatMap(small => all1LargePermuations.map(small ++ _))
//  private val all2Large4SmallCombinations: List[List[Int]] = all4SmallPermuations.flatMap(small => all2LargePermuations.map(small ++ _))
//  private val all3Large3SmallCombinations: List[List[Int]] = all3SmallPermuations.flatMap(small => all3LargePermuations.map(small ++ _))
//  private val all4Large2SmallCombinations: List[List[Int]] = all2SmallPermuations.flatMap(small => all4LargePermuations.map(small ++ _))

  private val all0Large6SmallCombinations: List[List[Int]] = all6SmallPermuations.map(_.sorted)
  private val all1Large5SmallCombinations: List[List[Int]] = all1LargePermuations.flatMap(large => all5SmallPermuations.map(_ ++ large)).map(_.sorted)
  private val all2Large4SmallCombinations: List[List[Int]] = all2LargePermuations.flatMap(large => all4SmallPermuations.map(_ ++ large)).map(_.sorted)
  private val all3Large3SmallCombinations: List[List[Int]] = all3LargePermuations.flatMap(large => all3SmallPermuations.map(_ ++ large)).map(_.sorted)
  private val all4Large2SmallCombinations: List[List[Int]] = all4LargePermuations.flatMap(large => all2SmallPermuations.map(_ ++ large)).map(_.sorted)
  private val allCombinations = all0Large6SmallCombinations ++ all1Large5SmallCombinations ++ all2Large4SmallCombinations ++ all3Large3SmallCombinations ++ all4Large2SmallCombinations

  private def generateNumberCombinations(
    total: Option[Int] = None,
    zeroLargeSixSmall: Int = 0,
    oneLargeFiveSmall: Int = 0,
    twoLargeFourSmall: Int = 0,
    threeLargeThreeSmall: Int = 0,
    fourLargeTwoSmall: Int = 0,
    shuffled: Boolean = true
  ): List[List[Int]] = {
    (total, shuffled) match {
      case (Some(t), true) =>
        Random.shuffle(allCombinations) take t
      case (None, _) =>
        ((if (shuffled) Random.shuffle(all0Large6SmallCombinations) else all0Large6SmallCombinations) take zeroLargeSixSmall) ++
          ((if (shuffled) Random.shuffle(all1Large5SmallCombinations) else all1Large5SmallCombinations) take oneLargeFiveSmall) ++
          ((if (shuffled) Random.shuffle(all2Large4SmallCombinations) else all2Large4SmallCombinations) take twoLargeFourSmall) ++
          ((if (shuffled) Random.shuffle(all3Large3SmallCombinations) else all3Large3SmallCombinations) take threeLargeThreeSmall) ++
          ((if (shuffled) Random.shuffle(all4Large2SmallCombinations) else all4Large2SmallCombinations) take fourLargeTwoSmall)
    }
  }

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

    val numbersCombinationsForExperiment = generateNumberCombinations()

    println(numbersCombinationsForExperiment)
    println("EXPERIMENT DONE")
  }

}
