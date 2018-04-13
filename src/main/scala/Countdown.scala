package main.scala

class Countdown(picked: List[Int], target: Int) {

    def pairwiseCalculationList(ints: (Int, Int)): Set[Int] = {
        Set(ints._1 + ints._2, ints._2 - ints._1, ints._1 * ints._2, ints._2 / ints._1)
                .filter(d => d > 0 && d % 1 == 0)
    }

    def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists
            .map(numberList => {
                numberList
                    .zipWithIndex
                    .map(d => {
                        (d._2 + 1 until numberList.size)
                            .map(i => pairwiseCalculationList((d._1, numberList(i))))
                            .zipWithIndex
                            .map(pairwiseList => {
                                pairwiseList._1
                                    .map(pairwiseValue => {
                                        val mem = numberList diff List(numberList(d._2), numberList(d._2 + 1 + pairwiseList._2))
                                        (mem :+ pairwiseValue).sorted
                                    })
                            })
                    })
            }).flatten.flatten.flatten
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
