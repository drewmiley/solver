package main

case class Calculation(values: List[Int], representation: List[String] = List.empty)

trait Operation {
  def value: Float
  def representation: String
}

case class AdditionOperation(min: Int, max: Int) extends Operation {
  override def value: Float = min + max
  override def representation: String = s"$min + $max = ${ min + max }"
}

case class SubtractionOperation(min: Int, max: Int) extends Operation {
  override def value: Float = max - min
  override def representation: String = s"$max - $min = ${ max - min }"
}

case class MultiplyOperation(min: Int, max: Int) extends Operation {
  override def value: Float = min * max
  override def representation: String = s"$min * $max = ${ min * max }"
}

case class DivideOperation(min: Int, max: Int) extends Operation {
  override def value: Float = max.toFloat / min
  override def representation: String =  s"$max / $min = ${ max / min }"
}

case class State(currentResult: List[Calculation], solutions: List[Calculation] = List.empty)