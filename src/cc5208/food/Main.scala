package cc5208.food

import scala.reflect.io.File
import scala.reflect.io.Path.string2path
import scala.collection.mutable.Queue

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
  
  def processData(db: DB) = junkFood(db)
  
  def setNutrients(db: DB, foods: List[FoodDescription], nutrients: List[NutrientDefinition]) = {
    val nutrientIds = nutrients.map(v => v.nutId)
    val nutrientsData = Food.getNutrients(db, foods.map(f => f.id), nutrientIds)
    val nutrientsLists = NutrientData.groupByFoodId(nutrientsData)
    foods.zip(nutrientsLists).foreach(f => f._1.nutrients = f._2)
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
      setNutrients(db, food.flatMap(f => f.states), vitamins)
      File(g._1 + " (vitamins).txt").writeAll(food)
    })
  }
  
  val junkBrands = List(
      "McDONALD",
      "BURGER",
      "WENDY",
      "TACO",
      "PIZZA",
      "DOMINO",
      "PAPA",
      "POPEYES")
  
  def prueba3(db: DB) = {
    Food.nutrientDefinitions.values.foreach(n => println(n.description))
  }
      
  def junkFood(db: DB) = {
    val junk = Food.allFromGroup(db, 2100)
    val foodByBrand = junkBrands map (jb => Pair(jb, junk.filter(j => j.name.contains(jb))))
    
    val salads = foodByBrand //.map(b => Pair(b._1, b._2.filter(f => f.description.toLowerCase contains "fries")))
    val s = salads.flatMap(p => p._2)    
    setNutrients(db, s, Food.nutrientDefinitions.values.toList)
    
    val nutrients = s.flatMap(f => f.nutrients).sortWith(_.nutId < _.nutId)
    
    var c = 0
    var lastId = 0
    val chosenNutrients = Queue[Option[NutrientDefinition]]()
    nutrients.foreach(n => {
      if (n.nutId == lastId) {
        c += 1
        if (c == s.size) chosenNutrients += Food.nutrientDefinitions.get(n.nutId)
      }
      else {
        lastId = n.nutId
        c = 1
      }
    })
   
    val nIds = chosenNutrients.map(n => n match {
      case Some(x) => x.nutId
      case None => 0
    })
    
    s.foreach(f => f.nutrients = f.nutrients.filter(n => nIds contains n.nutId))
  
    File("fries.okc").writeAll(toOKC(s))
    /*
    setNutrients(db, foodByBrand.head._2, Food.nutrientDefinitions.values.filter(n => n.nutId == 208).toList) // calorias
    println(foodByBrand.head._2)*/
  }
  
  def toOKC(foodList: List[FoodDescription]): String = {
    val names = foodList.head.nutrients.map(n => Food.nutrientDefinitions(n.nutId).description).reduce(_ + "\n" + _).replace(" ","_") +"\n"  
    val Nvals = foodList.map(f => {      f.nutrients.map(_.nutrVal)})
    val NvalsLists =  Nvals.transpose
    val minMax = NvalsLists.map(f => f.min +" "+ f.max+" "+"4").reduce(_ + "\n" + _)
    println(minMax)
    val data = foodList.map(f => {
      f.nutrients.map(_.nutrVal + " ").reduce(_ + _)
    }).reduce(_ + "\n" + _)
    
    foodList.head.nutrients.size+" "+foodList.size+"\n" + names + minMax + "\n" + data
  }
  
  implicit def listOfFoodDescToString(foodList: List[FoodDescription]): String = {
    foodList.map(f => f.toString()).reduce(_ + _)
  }
  
  implicit def listOfFoodsToString(foodsList: List[FoodUnit]): String = {
    foodsList.map(f => f.toString()).reduce(_ + _)
  }
}