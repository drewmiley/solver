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

  def printValue(name: String, output: String): Unit = println(s"$name | $output")
}
