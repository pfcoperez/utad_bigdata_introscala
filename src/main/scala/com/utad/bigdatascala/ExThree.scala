package com.utad.bigdatascala

import util.{Try, Failure}

object ExThree extends App {

  def readPopulationFile(path: String): Try[List[Person]] = Try {
    import io.Source

    val separator = "\","

    val lines = Source.fromFile(path).getLines.map(line => line.split(separator).map(_.tail)).toList

    val fieldNameToPosition = lines.head.zipWithIndex.toMap

    lines.tail map { splitted =>

      val Seq(name, age, email) = Seq("first_name","age","email").map(field => splitted(fieldNameToPosition(field)))

      Person(name, age.toInt, email)
    }

  }

  def writeResult(path: String, result: List[Person]): Try[Unit] = Try {
    import java.io._

    val writer = new BufferedWriter(new FileWriter(path))

    writer.write(""""first_name","age","email"""")

    result foreach {
      case Person(name, age, email) =>
        writer.newLine()
        writer.write(s""""$name","$age","$email"""")
    }

    writer.close()

  }

  val inputPath = "population.csv"
  val outputPath = "result.csv"

  val processingResult = for {

    population <- readPopulationFile(inputPath)

    filteredResult = population filter {
      case Person(_, age, email) =>
        age > 18 && email.endsWith(".com")
      case _ => false
    }

    outResult <- writeResult(outputPath, filteredResult)
  } yield ()

  println {
    processingResult match {
      case Failure(cause) => s"The process failed, cause: $cause"
      case _ => "OK"
    }
  }

}
