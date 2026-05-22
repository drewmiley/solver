package main

import Countdown.getNewState

object Demo {

  def runDemo(): Unit = {
    val pickedNumbers = List(1, 2, 3, 4, 6, 20)
    val targetNumber = 179

    val initGetNewState: State => State = getNewState(targetNumber, filterDuplicate = true)

    val stateStep0: State = State(List(Calculation(pickedNumbers)))
    // valuesLengthStep0 should be Set(6)
    val valuesLengthStep0: Set[Int] = stateStep0.currentResult.map(_.values).map(_.size).toSet
    println("STEP 0 DONE")

    val stateStep1: State = initGetNewState(stateStep0)
    // valuesLengthStep1 should be Set(5)
    val valuesLengthStep1: Set[Int] = stateStep1.currentResult.map(_.values).map(_.size).toSet
    println("STEP 1 DONE")

    val stateStep2: State = initGetNewState(stateStep1)
    // valuesLengthStep2 should be Set(4)
    val valuesLengthStep2: Set[Int] = stateStep2.currentResult.map(_.values).map(_.size).toSet
    println("STEP 2 DONE")

    val stateStep3: State = initGetNewState(stateStep2)
    // valuesLengthStep3 should be Set(3)
    val valuesLengthStep3: Set[Int] = stateStep3.currentResult.map(_.values).map(_.size).toSet
    println("STEP 3 DONE")

    val stateStep4: State = initGetNewState(stateStep3)
    // valuesLengthStep4 should be Set(2)
    val valuesLengthStep4: Set[Int] = stateStep4.currentResult.map(_.values).map(_.size).toSet
    println("STEP 4 DONE")

    val stateStep5: State = initGetNewState(stateStep4)
    // valuesLengthStep5 should be Set(1)
    val valuesLengthStep5: Set[Int] = stateStep5.currentResult.map(_.values).map(_.size).toSet
    println("STEP 5 DONE")

    val solutions = stateStep5.solutions
    println("SOLUTIONS FOUND")
  }

}
