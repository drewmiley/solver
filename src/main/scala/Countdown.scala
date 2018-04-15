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

    def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists.flatMap(numberList => {
            // TODO: Replace with generateIndexPairs iteration
            numberList.zipWithIndex
                .flatMap(d => {
                    (d._2 + 1 until numberList.size)
                        .map(i => (applyOperatorsToIntegerPair(d._1, numberList(i)), i))
                        .flatMap(pairwiseList => pairwiseList._1.map(pairwiseValue =>
                            ((numberList diff List(numberList(d._2), numberList(pairwiseList._2))) :+ pairwiseValue).sorted)
                    )
            })
        })
    }

    def runSolver(picked: List[Int], target: Int): List[List[Int]] = {
        val state = new State(List(picked))
        def recurse(state: State): State = state.currentResult.exists(d => d.size > 1) match {
            case false => state
            case true => {
                val calculatedValues = performPairwiseCalculations(state.currentResult)
                recurse(new State(calculatedValues.filter(d => !d.contains(target)),
                    state.solutions ++ calculatedValues.filter(d => d.contains(target))))
            }
        }
        recurse(state).solutions
    }

    def isSolvable(): Boolean = {
        val solutions = runSolver(picked, target)
        println(solutions)
        solutions.size > 0
    }
}
