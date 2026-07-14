package main

object Util {

  def getConfigBoolFromArgs(args: List[String], argKey: String): Option[Boolean] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.toBoolean) else None
    }
  }

  def getConfigIntFromArgs(args: List[String], argKey: String): Option[Int] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.toInt) else None
    }
  }

  def getConfigIntListFromArgs(args: List[String], argKey: String): Option[List[Int]] = {
    args.indexOf(argKey) match {
      case -1 => None
      case argKeyIndex =>
        val argValue = args.splitAt(argKeyIndex + 1)._2.head
        if (argValue.nonEmpty) Some(argValue.split(",").map(_.toInt).toList) else None
    }
  }

  def getPickedNumbers(picked: Option[List[Int]] = None,
                               smallRandom: Option[Int] = None,
                               largeRandom: Option[Int] = None): List[Int] = {
    picked match {
      case Some(intList) => intList.sorted
      case None =>
        val largeNumbers = largeRandom.getOrElse(1)
        val smallNumbers = smallRandom.getOrElse(6 - largeNumbers)

        val largePicker = new NumberPicker((1 to 4).map(d => 25 * d).toList)
        val smallPicker = new NumberPicker((1 to 10).toList ++ (1 to 10).toList)

        val pickedNumbers = smallPicker.select(smallNumbers).sorted ++ largePicker.select(largeNumbers)
        pickedNumbers
    }
  }

  def printValue(name: String, output: String): Unit = println(s"$name | $output")
}
