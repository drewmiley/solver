package main.scala

case class Calculation(values: List[Int], representation: List[String] = List())

case class Operation(value: Float, representation: String = "")

case class State(currentResult: List[Calculation] = List(), solutions: List[Calculation] = List())

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
        val min = numberList.values(indexPair._1)
        val max = numberList.values(indexPair._2)
        val integerPairOperationValues = applyOperatorsToIntegerPair(min, max)
        val listWithIndexPairRemoved = numberList.values diff List(min, max)
        integerPairOperationValues.map(operation =>
            Calculation((listWithIndexPairRemoved :+ operation.value.toInt).sorted,
                numberList.representation :+ operation.representation)
        )
    }

    def performOneOperationOnCurrentLists(listOfNumberLists: List[Calculation]): List[Calculation] = {
        listOfNumberLists.flatMap(numberList => {
            generateIndexPairs(numberList.values.size).flatMap(indexPair => {
                operateOnIntegerPairAndCreateNewLists(numberList, indexPair)
            })
        })
    }

    def solve(picked: List[Int], target: Int): State = {
        val state = new State(List(Calculation(picked)))
        def recurse(state: State): State = if (state.currentResult.map(_.values).exists(d => d.size > 1)) {
            val calculatedValues = performOneOperationOnCurrentLists(state.currentResult)
            recurse(new State(calculatedValues.filter(d => !d.values.contains(target)),
                state.solutions ++ calculatedValues.filter(d => d.values.contains(target))))
        } else {
            state
        }
        recurse(state)
    }

    def formPrintableSolutions(solutions: List[Calculation]): List[String] = solutions.map(_.representation.mkString("Solved | ", ", ", ""))
}
