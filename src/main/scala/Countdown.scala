package main.scala

case class Calculation(values: List[Int], representation: List[String] = List())

case class Operation(value: Float, representation: String)

case class State(currentResult: List[Calculation], solutions: List[Calculation] = List())

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

    def solve(picked: List[Int], target: Int): State = {
        val state: State = State(List(Calculation(picked)))
        def recurse(state: State): State = if (state.currentResult.map(_.values).exists(_.size > 1)) {
            val calculatedValues: List[Calculation] = performOneOperationOnCurrentLists(state.currentResult)
            recurse(
                State(
                    calculatedValues.filter(!_.values.contains(target)),
                    state.solutions ++ calculatedValues.filter(_.values.contains(target))
                )
            )
        } else {
            state
        }
        recurse(state)
    }

    def formPrintableSolutions(solutions: List[Calculation]): List[String] = solutions.map(_.representation.mkString("Solved | ", ", ", ""))
}
