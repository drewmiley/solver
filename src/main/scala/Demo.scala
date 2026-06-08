package main

import CountdownSolutions.initGetNewState

object Demo {

  private def initPrintStateStepInfo(pickedNumbersLength: Int)(stateAtStep: SolutionsState, stepNumber: Int): Unit = {
      println("---------------")
      println(s"STEP $stepNumber DONE")
      println(s"stateStep$stepNumber has currentResult size ${stateAtStep.currentResult.length}")
      println(s"stateStep$stepNumber has solutions size ${stateAtStep.solutions.length}")
      val valuesLengthAtStep: Set[Int] = stateAtStep.currentResult.map(_.values).map(_.size).toSet
      println(s"valuesLengthStep$stepNumber should be Set(${pickedNumbersLength - stepNumber})")
      println(s"valuesLengthStep$stepNumber is $valuesLengthAtStep")
  }

  def runDemo(): Unit = {
    val pickedNumbers = List(1, 2, 3, 4, 6, 20)
    val targetNumber = 179
//    val filterDuplicate = true
    val filterDuplicate = false

//    NOTE: Top of the head says complexity 1.5 to power n times n! squared - this is high!!! (for filterDuplicate false)
    val getNewState: SolutionsState => SolutionsState = initGetNewState(targetNumber, filterDuplicate = filterDuplicate)
    println(s"Picked | ${pickedNumbers.mkString(", ")}")
    println(s"Target | $targetNumber")
    println(s"filterDuplicate | $filterDuplicate")

    val printStateStepInfo: (SolutionsState, Int) => Unit = initPrintStateStepInfo(pickedNumbers.length)

    val stateStep0: SolutionsState = SolutionsState(List(Calculation(pickedNumbers)))
    printStateStepInfo(stateStep0, 0)

    val stateStep1: SolutionsState = getNewState(stateStep0)
    printStateStepInfo(stateStep1, 1)

    val stateStep2: SolutionsState = getNewState(stateStep1)
    printStateStepInfo(stateStep2, 2)

    val stateStep3: SolutionsState = getNewState(stateStep2)
    printStateStepInfo(stateStep3, 3)

    val stateStep4: SolutionsState = getNewState(stateStep3)
    printStateStepInfo(stateStep4, 4)

    val stateStep5: SolutionsState = getNewState(stateStep4)
    printStateStepInfo(stateStep5, 5)

    val solutions = stateStep5.solutions
    println("---------------")
    println("SOLUTIONS FOUND")
  }

}
