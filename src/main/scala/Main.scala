package main

object Main {

  private def printValue(name: String, output: String): Unit = println(s"$name | $output")

  private def formPrintableSolutions(solutions: List[Calculation]): List[String] = solutions.map(_.representation.mkString("Solved | ", ", ", ""))

  def main(args: Array[String]): Unit = {
    val argsList = args.toList

    val picked: Option[List[Int]] = getConfigIntListFromArgs(argsList, "picked")
    val smallRandom: Option[Int] = getConfigIntFromArgs(argsList, "smallRandom")
    val largeRandom: Option[Int] = getConfigIntFromArgs(argsList, "largeRandom")
    val target : Option[Int] = getConfigIntFromArgs(argsList, "target")
    val filterDuplicate: Boolean = getConfigBoolFromArgs(argsList, "filterDuplicate").getOrElse(true)

//    TODO: Temp for local testing of remove duplicates
//    val solutions: List[Calculation] = findSolutions(picked, smallRandom, largeRandom, target, filterDuplicate)
    val solutions: List[Calculation] = findSolutions(Some(List(1,2,3,4,6,20)), smallRandom, largeRandom, Some(179), filterDuplicate)
    solutions.map(_.representation.mkString(", ")).foreach(printValue("Solved", _))
//    formPrintableSolutions(solutions).foreach(println)
  }

  private def getConfigBoolFromArgs(args: List[String], argKey: String): Option[Boolean] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.toBoolean) else None
    }
  }

  private def getConfigIntFromArgs(args: List[String], argKey: String): Option[Int] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.toInt) else None
    }
  }

  private def getConfigIntListFromArgs(args: List[String], argKey: String): Option[List[Int]] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.split(",").map(_.toInt).toList) else None
    }
  }

  private def findSolutions(picked: Option[List[Int]] = None,
                            smallRandom: Option[Int] = None,
                            largeRandom: Option[Int] = None,
                            target : Option[Int] = None,
                            filterDuplicate: Boolean = true): List[Calculation] = {
    def getPickedNumbers: List[Int] = {
      picked match {
        case Some(intList) => intList
        case None =>
          val largeNumbers = largeRandom.getOrElse(1)
          val smallNumbers = smallRandom.getOrElse(6 - largeNumbers)

          val largePicker = new NumberPicker((1 to 4).map(d => 25 * d).toList)
          val smallPicker = new NumberPicker((1 to 10).toList ++ (1 to 10).toList)

          val pickedNumbers = smallPicker.select(smallNumbers).sorted ++ largePicker.select(largeNumbers)
          pickedNumbers
      }
    }

    def getTargetNumber: Int = {
      target match {
        case Some(value) => value
        case None =>
          val answerPicker = new NumberPicker((101 to 999).toList)
          val targetNumber = answerPicker.select(1).head
          targetNumber
      }
    }

    val pickedNumbers = getPickedNumbers
    printValue("Picked", pickedNumbers.mkString(", "))

    val targetNumber = getTargetNumber
    printValue("Target", targetNumber.toString)

    val solutions: List[Calculation] = Countdown.solve(pickedNumbers, targetNumber, filterDuplicate).solutions
    solutions
  }
}
