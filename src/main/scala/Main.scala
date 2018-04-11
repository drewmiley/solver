package main.scala

object Main extends App {
    val largeNumbers = 1
    val smallNumbers = 6 - largeNumbers

    val largePicker = new NumberPicker((1 to 4).map(d => 25 * d).toList)
    val smallPicker = new NumberPicker((1 to 10).toList ++ (1 to 10).toList)

    val answerPicker = new NumberPicker((101 to 999).toList)

    val pickedNumbers = largePicker.select(largeNumbers) ++ smallPicker.select(smallNumbers)

    println(pickedNumbers)

    val targetNumber = answerPicker.select(1).head

    println(targetNumber)
}
