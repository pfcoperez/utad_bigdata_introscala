package com.utad.bigdatascala

object ExTwo extends App {

  val minAge = 18

  // Let's generate a bunch of 10 persons, some of then sharing age
  val people = (1 to 10) map { n =>
    Person(s"Person$n", minAge + n % 3, s"person$n@u-tad.com")
  } toList

  { //Non functional approach: Creating an empty map and updating it
    import collection.mutable.{Map, ArrayBuffer}

    val resMap: Map[Int, ArrayBuffer[Person]] = Map.empty

    people foreach { case person @ Person(_, age, _) =>
      if(resMap contains age)
        resMap(age) += person
      else
        resMap += (age -> ArrayBuffer(person))
    }

    println(resMap)

  }

  { //Functional approach: Creating a map using higher order methods
    val resMap: Map[Int, List[Person]] =
      people.foldRight(Map.empty[Int, List[Person]]) {
        case (person @ Person(_, age, _), prevMap) =>
          val newOrUpdatedEntry = prevMap.get(age) map { prevList => // If the age was already in the map,..
            person +: prevList //then we have to provide an updated list.
          } getOrElse Nil //otherwise, an empty list.
          prevMap + (age -> newOrUpdatedEntry)
      }

    println(resMap)

  }

}
