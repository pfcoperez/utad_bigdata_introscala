package com.utad.bigdatascala

object ExOne extends App {

  def invertAndMap[T, S](l: List[T], transformFunction: T => S): Map[T, S] = {
    l.reverse.foldLeft(Map.empty[T, S]) { (previousMap, v) => //This block defines a two parameter function...
                                                              //the previous state of the map and the next value in the iteration...
                                                              //it then returns an updated version the the previous map.
      previousMap + (v -> transformFunction(v)) // Writing `a -> b` is just another way of creating a tuple: `(a, b)`
    }
  }

  //This function takes an integer and returns its string representation
  //val transformToString: Int => String = x => x.toString

  //It could as well have been written as:
  //val transformToString: Int => String = _.toString

  //Or be defined as a method:
  def transformToString(x: Int): String = x.toString


  val l = List(1,2,3,4,5)

  val r1 = invertAndMap(l, transformToString)
  println(r1)

  //Let's define square as a method
  def square(x: Int): Int = x*x

  val r2 = invertAndMap(l, square)
  println(r2)

}
