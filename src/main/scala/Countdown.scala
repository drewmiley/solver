package main

import scala.annotation.tailrec

case class Calculation(values: List[Int], representation: List[String] = List())

case class Operation(value: Float, representation: String)

case class State(currentResult: List[Calculation], solutions: List[Calculation] = List())

// TODO: Annotated example of this working step-by-step
object Countdown {

  def applyOperatorsToIntegerPair(min: Int, max: Int): List[Operation] = {
    List(
      Operation(min + max, s"$min + $max = ${ min + max }"),
      Operation(max - min, s"$max - $min = ${ max - min }"),
      Operation(min * max, s"$min * $max = ${ min * max }"),
      Operation(max.toFloat / min, s"$max / $min = ${ max / min }")
    ).filter(operation => operation.value > 0 && operation.value % 1 == 0 &&
      operation.value != min && operation.value != max)
  }

  def generateIndexPairs(listSize: Int): List[(Int, Int)] = {
    (0 until listSize).flatMap(i => (i + 1 until listSize).map(j => (i, j))).toList
  }

  def operateOnIntegerPairAndCreateNewLists(numberList: Calculation, indexPair: (Int, Int)): List[Calculation] = {
    val min: Int = numberList.values(indexPair._1)
    val max: Int = numberList.values(indexPair._2)
    val listWithIndexPairRemoved: List[Int] = numberList.values diff List(min, max)
    applyOperatorsToIntegerPair(min, max).map(operation =>
      Calculation(
        (listWithIndexPairRemoved :+ operation.value.toInt).sorted,
        numberList.representation :+ operation.representation
      )
    )
  }

  def performOneOperationOnCurrentLists(listOfNumberLists: List[Calculation]): List[Calculation] =
    listOfNumberLists
      .flatMap(numberList =>
        generateIndexPairs(numberList.values.size)
          .flatMap(operateOnIntegerPairAndCreateNewLists(numberList, _))
      )

  private def removeDuplicates(calculatedValues: List[Calculation]): List[Calculation] = {
    //TODO: Is there a way to filter duplicate solutions - possibly multiple additions, subtractions, multiplications, divisions - 3 numbers -> 1 with symmetry?
    //List types of duplication here. String are equal, two adds, two multiplies, two divides.
    //Good example to use on this is: run picked 1,2,3,4,6,20 target 179
    println("CHECK HERE")
    calculatedValues
  }

  def solve(picked: List[Int], target: Int): State = {
    val state: State = State(List(Calculation(picked)))
    @tailrec
    def recurse(state: State): State = if (state.currentResult.map(_.values).exists(_.size > 1)) {
      val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
      val currentCalculationsWithDuplicatesRemoved: List[Calculation] = removeDuplicates(calculatedValues)
      recurse(
        State(
          currentCalculationsWithDuplicatesRemoved.filter(!_.values.contains(target)),
          state.solutions ++ currentCalculationsWithDuplicatesRemoved.filter(_.values.contains(target))
        )
      )
    } else {
      state
    }
    recurse(state)
  }

  def formPrintableSolutions(solutions: List[Calculation]): List[String] = solutions.map(_.representation.mkString("Solved | ", ", ", ""))
}
