package main.scala

class Countdown(picked: List[Int], target: Int) {
    private class State(var currentResult: List[List[Int]] = List(),
                        var solutions: List[List[Int]] = List())

    def applyOperatorsToIntegerPair(min: Int, max: Int): Set[Int] = {
        (Set(min + max, max - min, min * max, max / min) diff Set(min, max))
            .filter(d => d > 0 && d % 1 == 0)
    }

    def generateIndexPairs(listSize: Int): List[(Int, Int)] = {
        (0 until listSize).flatMap(i => (i + 1 until listSize).map(j => (i, j))).toList
    }

    def operateOnIntegerPairAndCreateNewLists(numberList: List[Int], indexPair: (Int, Int)): List[List[Int]] = {
        val min = numberList(indexPair._1)
        val max = numberList(indexPair._2)
        val integerPairOperationValues = applyOperatorsToIntegerPair(min, max)
        val listWithIndexPairRemoved = numberList diff List(min, max)
        integerPairOperationValues.toList.map(value => (listWithIndexPairRemoved :+ value).sorted)
    }

    def performOneOperationOnCurrentLists(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists.flatMap(numberList => {
            generateIndexPairs(numberList.size).flatMap(indexPair => {
                operateOnIntegerPairAndCreateNewLists(numberList, indexPair)
            })
        })
    }

    def runSolver(picked: List[Int], target: Int): List[List[Int]] = {
        val state = new State(List(picked))
        def recurse(state: State): State = if (state.currentResult.exists(d => d.size > 1)) {
            val calculatedValues = performOneOperationOnCurrentLists(state.currentResult)
            recurse(new State(calculatedValues.filter(d => !d.contains(target)),
                state.solutions ++ calculatedValues.filter(d => d.contains(target))))
        } else {
            state
        }
        recurse(state).solutions
    }

    def solutions: List[String] = runSolver(picked, target).map(solution => "Solved")
}
