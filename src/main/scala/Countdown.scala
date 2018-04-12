package main.scala

class Countdown(picked: List[Int], target: Int) {

    def pairwiseCalculationList(ints: (Int, Int)): List[Int] = {
        List(ints._1 + ints._2, ints._2 - ints._1, ints._1 * ints._2, ints._2 / ints._1)
                .filter(d => d > 0 && d % 1 == 0)
    }

    def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        val tupleNumberLists = listOfNumberLists
            .map(numberList => {
                numberList
                    .zipWithIndex
                    .map(d => {
                        d._1
                    })
            })
        println(tupleNumberLists)
        listOfNumberLists.map(d => d diff List(d.head))
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
        savedValues.size > 0
    }
}
