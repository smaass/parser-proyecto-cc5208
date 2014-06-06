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
    val nutrList = nutrHash.values.toList
    
    val vitamins = nutrList.filter(n => n.description contains "Vitamin")    
    val vegetables = FoodDescription.groupByName(Food.allFromGroup(db, 1100)).filter(g => g.size > 1)
    
    val t0 = System.nanoTime
    setNutrients(db, vegetables, vitamins)    
    val dt = (System.nanoTime - t0)/1000000
    println("Listo :D! " + dt + "ms")
  }

  def setNutrients(db: DB, foods: List[List[FoodDescription]], nutrients: List[NutrientDefinition]) = {
    val nutrientIds = nutrients.map(v => v.nutId)
    val nutrientsData = Food.getNutrients(db, foods.flatMap(f => f.map(s => s.id)), nutrientIds)
    val nutrientsLists = NutrientData.groupByFoodId(nutrientsData)
    foods.flatten.zip(nutrientsLists).foreach(f => f._1.nutrients = f._2)
    foods
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
      File(f._1 + ".txt").writeAll(FoodDescription.groupByName(data))
    })
    
    println("Listo :)")
  }
  
  implicit def listOfDataListsToString(dataLists: List[List[FoodDescription]]): String = {
    dataLists.map(g => 
      "{\n" + 
      g.map(f => "\t" + f + "\n").reduce(_ + _) +
      "},\n"
    ).reduce(_ + _)
  }
}