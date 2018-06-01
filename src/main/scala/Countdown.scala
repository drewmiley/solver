package main.scala

case class Calculation(values: List[Int], representation: String = "")

class Countdown(picked: List[Int], target: Int) {
    private case class State(currentResult: List[Calculation] = List(), solutions: List[Calculation] = List())


    def applyOperatorsToIntegerPair(min: Int, max: Int): Set[Int] = {
        (Set(min + max, max - min, min * max, max / min) diff Set(min, max))
            .filter(d => d > 0 && d % 1 == 0)
    }

    def generateIndexPairs(listSize: Int): List[(Int, Int)] = {
        (0 until listSize).flatMap(i => (i + 1 until listSize).map(j => (i, j))).toList
    }

    def operateOnIntegerPairAndCreateNewLists(numberList: Calculation, indexPair: (Int, Int)): List[Calculation] = {
        val min = numberList.values(indexPair._1)
        val max = numberList.values(indexPair._2)
        val integerPairOperationValues = applyOperatorsToIntegerPair(min, max)
        val listWithIndexPairRemoved = numberList.values diff List(min, max)
        integerPairOperationValues.toList.map(value => Calculation((listWithIndexPairRemoved :+ value).sorted))
    }

    def performOneOperationOnCurrentLists(listOfNumberLists: List[Calculation]): List[Calculation] = {
        listOfNumberLists.flatMap(numberList => {
            generateIndexPairs(numberList.values.size).flatMap(indexPair => {
                operateOnIntegerPairAndCreateNewLists(numberList, indexPair)
            })
        })
    }

    def runSolver(picked: List[Int], target: Int): List[Calculation] = {
        val state = new State(List(Calculation(picked)))
        def recurse(state: State): State = if (state.currentResult.map(_.values).exists(d => d.size > 1)) {
            val calculatedValues = performOneOperationOnCurrentLists(state.currentResult)
            recurse(new State(calculatedValues.filter(d => !d.values.contains(target)),
                state.solutions ++ calculatedValues.filter(d => d.values.contains(target))))
        } else {
            state
        }
        recurse(state).solutions
    }

    def solutions: List[String] = runSolver(picked, target).map(solution => "Solved")
}
