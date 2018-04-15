package main.scala

class Countdown(picked: List[Int], target: Int) {

    private class State(var calculatedValues: List[List[Int]] = List(),
                        var currentResult: List[List[Int]] = List(),
                        var solutions: List[List[Int]] = List())

    private def pairwiseCalculationSet(min: Int, max: Int): Set[Int] = {
        (Set(min + max, max - min, min * max, max / min) diff Set(min, max))
            .filter(d => d > 0 && d % 1 == 0)
    }

    private def getNewList(fullList: List[Int], firstIndex: Int, secondIndex: Int, newValue: Int): List[Int] = {
        ((fullList diff List(fullList(firstIndex), fullList(secondIndex))) :+ newValue).sorted
    }

    private def generateIndexPairs(listSize: Int): List[(Int, Int)] = {
        (0 until listSize).flatMap(i => (i + 1 until listSize).map(j => (i, j))).toList
    }

    private def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists.flatMap(numberList => {
            // TODO: Replace with generateIndexPairs iteration
            numberList.zipWithIndex
                .flatMap(d => {
                    (d._2 + 1 until numberList.size)
                        .map(i => (pairwiseCalculationSet(d._1, numberList(i)), i))
                        .flatMap(pairwiseList => pairwiseList._1.map(pairwiseValue =>
                            getNewList(numberList, d._2, pairwiseList._2, pairwiseValue))
                    )
            })
        })
    }

    private def iterate(state: State): State = {
        var solutions = state.solutions
        var currentResult = state.currentResult
        var calculatedValues = state.calculatedValues
        calculatedValues = performPairwiseCalculations(currentResult)
        currentResult = calculatedValues.filter(d => !d.contains(target))
        solutions = solutions ++ calculatedValues.filter(d => d.contains(target))
        new State(calculatedValues, currentResult, solutions)
    }

    private def runSolver(initialState: State): State = {
        var state = initialState
        def recurse(state: State): State = state.currentResult.exists(d => d.size > 1) match {
            case false => state
            case true => recurse(iterate(state))
        }
        recurse(state)
    }

    def isSolvable(): Boolean = {
        var initialState = new State(List(), List(picked), List())
        val state = runSolver(initialState)
        println(state.solutions)
        state.solutions.size > 0
    }
}
