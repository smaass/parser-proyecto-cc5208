package cc5208.food

import scala.reflect.io.File
import scala.reflect.io.Path.string2path

object Main {
  
  def main(args: Array[String]) = {
    DB.connect match {
      case Some(db) => {
        processData(db)
        db.close
      }
      case None => println("No se pudo conectar a la base de datos")
    }
  }
  
  def processData(db: DB) = {
    val nutrHash = Food.nutrientDefinitions(db).map(n => n.nutId -> n).toMap
    val nutrList = nutrHash.values
    
    val eAminoacids = nutrList.filter(n => Food.essentialAminoacids contains n.description)
    eAminoacids.foreach(a => println(a))
  }
  
  def prueba1(db: DB) = {
    
    val groups = List(
        Pair("vegetables", 1100),
        Pair("legumes", 1600),
        Pair("poultry", 500),
        Pair("pork", 1000),
        Pair("beef", 1300),
        Pair("fish", 1500),
        Pair("lambVealGame", 1700))
    
    groups.foreach(f => {
      val data = Food.allFromGroup(db, f._2)
      File(f._1 + ".txt").writeAll(Data.groupByName(data))
    })
    
    println("Listo :)")
  }
  
  implicit def listOfDataListsToString(dataLists: List[List[HasName]]): String = {
    dataLists.map(g => 
      "{\n" + 
      g.map(f => "\t" + f + "\n").reduce(_ + _) +
      "},\n"
    ).reduce(_ + _)
  }
}