package main.scala

object Main extends App {
    val largeNumbers = 1
    val smallNumbers = 6 - largeNumbers
    println("Solved")
    println("Large")
    val largePicker = new NumberPicker((1 to 4).map(d => 25 * d).toList)
    println(largePicker.select(largeNumbers))
    println("Small")
    val smallPicker = new NumberPicker((1 to 10).toList ++ (1 to 10).toList)
    println(smallPicker.select(smallNumbers))
}
