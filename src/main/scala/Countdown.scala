package main.scala

class Countdown(picked: List[Int], target: Int) {

    private def pairwiseCalculationSet(ints: (Int, Int)): Set[Int] = {
        // TODO: Rewrite as two int parameters.
        // TODO: diff set rather than filter out parameters
        Set(ints._1 + ints._2, ints._2 - ints._1, ints._1 * ints._2, ints._2 / ints._1)
            .filter(d => d > 0 && d % 1 == 0 && d != ints._1 && d != ints._2)
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
                        .map(i => (pairwiseCalculationSet((d._1, numberList(i))), i))
                        .flatMap(pairwiseList => pairwiseList._1.map(pairwiseValue =>
                            getNewList(numberList, d._2, pairwiseList._2, pairwiseValue))
                    )
            })
        })
    }

    private def recurse(state: List[List[List[Int]]]): List[List[List[Int]]] = {
        // TODO: Rewrite using simple class
        // TODO: Rename saved values to solutions
        var savedValues = state(0)
        var currentResult = state(1)
        var calculatedValues = state(2)
        calculatedValues = performPairwiseCalculations(currentResult)
        currentResult = calculatedValues.filter(d => !d.contains(target))
        savedValues = savedValues ++ calculatedValues.filter(d => d.contains(target))
        List(savedValues, currentResult, calculatedValues)
    }

    def isSolvable(): Boolean = {
        // TODO: Rewrite as simple class
        var state: List[List[List[Int]]] = List(List(), List(picked), List())
        // TODO: Rewrite as sleek recursion
        while (state(1).exists(d => d.size > 1)) {
            state = recurse(state)
        }
        println(state(0))
        state(0).size > 0
    }
}
