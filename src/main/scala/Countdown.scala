package main.scala

class Countdown(picked: List[Int], target: Int) {

    def pairwiseCalculationList(a: Int, b: Int): List[Int] = {
        List(a + b, a - b, a * b, a / b).filter(d => d > 0 && d % 1 == 0)
    }

    def performPairwiseCalculations(listOfNumberLists: List[List[Int]]): List[List[Int]] = {
        listOfNumberLists
    }

    def isSolvable(): Boolean = {
        var savedValues: List[List[Int]] = List(picked)
        var currentResult = savedValues
        var calculatedValues = performPairwiseCalculations(currentResult)
        savedValues = savedValues ++ calculatedValues
        currentResult = calculatedValues
        savedValues.filter(d => d.contains(target)).length > 0
    }
}
