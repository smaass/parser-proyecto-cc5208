package cc5208.food

import scala.reflect.io.File
import scala.reflect.io.Path.string2path

object Main {
  
  def main(args: Array[String]) = {
    DB.connect match {
      case Some(db) => {
        Food.nutrientDefinitions = fetchNutrientDefinitions(db)
        processData(db)
        db.close
        println("Listo :)")
      }
      case None => println("No se pudo conectar a la base de datos")
    }
  }
  
  def fetchNutrientDefinitions(db: DB) = Food.nutrientDefinitions(db).map(n => n.nutId -> n).toMap
  
  def processData(db: DB) = prueba2(db)

  def setNutrients(db: DB, foods: List[FoodUnit], nutrients: List[NutrientDefinition]) = {
    val nutrientIds = nutrients.map(v => v.nutId)
    val nutrientsData = Food.getNutrients(db, foods.flatMap(f => f.states.map(s => s.id)), nutrientIds)
    val nutrientsLists = NutrientData.groupByFoodId(nutrientsData)
    foods.flatMap(f => f.states).zip(nutrientsLists).foreach(f => f._1.nutrients = f._2)
    foods
  }
  
  def time[A](f: => A): A = {
    val t0 = System.nanoTime
    val ret = f
    val dt = (System.nanoTime - t0)/1000000
    println("Computation:\n\t" + ret + "\nElapsed time: " + dt + "ms")
    ret
  }
  
  val foodGroups = List(
      Pair("Vegetables", 1100),
      Pair("Legumes", 1600),
      Pair("Poultry", 500),
      Pair("Pork", 1000),
      Pair("Beef", 1300),
      Pair("Finfish and Shellfish", 1500),
      Pair("Lamb, Veal and Game", 1700))
  
  def prueba1(db: DB) = {        
    foodGroups.foreach(f => {
      File(f._1 + ".txt").writeAll(FoodUnit.groupStates(Food.allFromGroup(db, f._2)))
    })
  }
      
  def prueba2(db: DB) = {
    val vitamins = Food.nutrientDefinitions.values.filter(n => n.description contains "Vitamin").toList
    foodGroups.take(2).foreach(g => {
      val food = FoodUnit.groupStates(Food.allFromGroup(db, g._2)).filter(g => g.states.size > 1)
      setNutrients(db, food, vitamins)
      File(g._1 + " (vitamins).txt").writeAll(food)
    })
  }
  
  implicit def listOfFoodsToString(foodsList: List[FoodUnit]): String = {
    foodsList.map(f => f.toString()).reduce(_ + _)
  }
}