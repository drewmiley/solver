package main.scala

import scala.util.Random

class NumberPicker(values: List[Int]) {

    def select(selections: Int): List[Int] = {
        Random.shuffle(values) take selections
    }
}