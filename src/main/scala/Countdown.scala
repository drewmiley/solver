package main.scala

class Countdown(picked: List[Int], target: Int) {

    def pairwiseCalculationList(a: Int, b: Int): List[Int] = {
        List(a + b, a - b, a * b, a / b).filter(d => d > 0 && d % 1 == 0)
    }

    def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists.map(d => d diff List(d.head))
    }

    def isSolvable(): Boolean = {
        var savedValues: List[List[Int]] = List(picked)
        var currentResult = savedValues
        var calculatedValues = List(List(0))
        while (currentResult.exists(d => d.size > 1)) {
            calculatedValues = performPairwiseCalculations(currentResult)
            currentResult = calculatedValues
            savedValues = savedValues ++ calculatedValues
        }
        savedValues.exists(d => d.contains(target))
    }
}
