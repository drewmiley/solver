package main.scala

class Countdown(picked: List[Int], target: Int) {

    private def pairwiseCalculationList(ints: (Int, Int)): Set[Int] = {
        Set(ints._1 + ints._2, ints._2 - ints._1, ints._1 * ints._2, ints._2 / ints._1)
            .filter(d => d > 0 && d % 1 == 0)
    }

    private def getNewList(numberList: List[Int], d: (Int, Int), pairwiseList: (Set[Int], Int), pairwiseValue: Int): List[Int] = {
        val mem = numberList diff List(numberList(d._2), numberList(d._2 + 1 + pairwiseList._2))
        (mem :+ pairwiseValue).sorted
    }

    private def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists.flatMap(numberList => {
            numberList.zipWithIndex
                .flatMap(d => {
                    (d._2 + 1 until numberList.size)
                        .map(i => pairwiseCalculationList((d._1, numberList(i)))).zipWithIndex
                        .flatMap(pairwiseList => pairwiseList._1.map(pairwiseValue => getNewList(numberList, d, pairwiseList, pairwiseValue))
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
