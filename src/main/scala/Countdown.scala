package main.scala

class Countdown(picked: List[Int], target: Int) {

    private def pairwiseCalculationSet(ints: (Int, Int)): Set[Int] = {
        Set(ints._1 + ints._2, ints._2 - ints._1, ints._1 * ints._2, ints._2 / ints._1)
            .filter(d => d > 0 && d % 1 == 0)
    }

    private def getNewList(fullList: List[Int], firstIndex: Int, secondIndex: Int, newValue: Int): List[Int] = {
        val mem = fullList diff List(fullList(firstIndex), fullList(secondIndex))
        (mem :+ newValue).sorted
    }

    private def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists.flatMap(numberList => {
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

    def isSolvable(): Boolean = {
        var savedValues: List[List[Int]] = List()
        var currentResult = List(picked)
        var calculatedValues = List(List(0))
        while (currentResult.exists(d => d.size > 1)) {
            calculatedValues = performPairwiseCalculations(currentResult)
            currentResult = calculatedValues.filter(d => !d.contains(target))
            savedValues = savedValues ++ calculatedValues.filter(d => d.contains(target))
        }
        println(savedValues)
        savedValues.size > 0
    }
}
