package main

import scala.collection.mutable

object Operations {

  def generateIndexPairs(listSize: Int): List[(Int, Int)] =
    (0 until listSize).flatMap(i => (i + 1 until listSize).map(j => (i, j))).toList

  private val indexPairsListSize2 = generateIndexPairs(2)
  private val indexPairsListSize3 = generateIndexPairs(3)
  private val indexPairsListSize4 = generateIndexPairs(4)
  private val indexPairsListSize5 = generateIndexPairs(5)
  private val indexPairsListSize6 = generateIndexPairs(6)

  private def getIndexPairs(listSize: Int): List[(Int, Int)] = {
    listSize match {
      case 2 => indexPairsListSize2
      case 3 => indexPairsListSize3
      case 4 => indexPairsListSize4
      case 5 => indexPairsListSize5
      case 6 => indexPairsListSize6
      case _ => generateIndexPairs(listSize)
    }
  }

  private val commonNumbersList: List[Int] = List(1,2,3,4,5,6,10,100)
  private val indexPairsForCommonNumbersList: List[(Int, Int)] = getIndexPairs(commonNumbersList.length)
  private var operationsOnCommonNumbersMap: collection.mutable.Map[(Int, Int), List[Operation]] = indexPairsForCommonNumbersList.map(indexPair => {
    val min = commonNumbersList(indexPair._1)
    val max = commonNumbersList(indexPair._2)
    (min, max) -> applyOperatorsToIntegerPair(min, max)
  }).to(mutable.Map)

  def applyOperatorsToIntegerPair(min: Int, max: Int): List[Operation] = {
    List(
      AdditionOperation(min, max),
      SubtractionOperation(min, max),
      MultiplyOperation(min, max),
      DivideOperation(min, max)
    ).filter(operation => operation.value > 0 && operation.value % 1 == 0 &&
      operation.value != min && operation.value != max)
  }

  private def applyOperatorsToIntegerPairWithCache(min: Int, max: Int): List[Operation] = {
    val cacheValue: Option[List[Operation]] = operationsOnCommonNumbersMap.get((min, max))
    cacheValue.getOrElse({
      val operatorsForIntegerPair = applyOperatorsToIntegerPair(min, max)
      operationsOnCommonNumbersMap += (min, max) -> operatorsForIntegerPair
      operatorsForIntegerPair
    })
  }

  def operateOnIntegerPairAndCreateNewLists(numberList: Calculation, indexPair: (Int, Int)): List[Calculation] = {
    val min: Int = numberList.values(indexPair._1)
    val max: Int = numberList.values(indexPair._2)
    val listWithIndexPairRemoved: List[Int] = numberList.values diff List(min, max)
    applyOperatorsToIntegerPairWithCache(min, max).map(operation =>
      Calculation(
        (listWithIndexPairRemoved :+ operation.value.toInt).sorted,
        numberList.representation :+ operation.representation
      )
    )
  }

  def performOneOperationOnCurrentLists(listOfNumberLists: List[Calculation]): List[Calculation] =
    listOfNumberLists
      .flatMap(numberList =>
        getIndexPairs(numberList.values.size)
          .flatMap(operateOnIntegerPairAndCreateNewLists(numberList, _))
      )

  def filterDuplicateCalculations(calculations: List[Calculation]): List[Calculation] = {
    val calculationsGroupedByValues: Map[List[Int], List[Calculation]] = calculations.groupBy(_.values)
    calculationsGroupedByValues.map(_._2.head).toList
  }
}
