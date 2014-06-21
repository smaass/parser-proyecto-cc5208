package cc5208.food

import scala.collection.mutable.Queue

abstract class Data

object FoodUnit {
  def groupStates(dataList: List[FoodDescription]): List[FoodUnit] = {
    val groupedData = Queue[FoodUnit]()
    val dataNames = dataList.map(f => f.name).distinct
    
    dataNames.foreach(n => groupedData += FoodUnit(dataList.filter(f => f.name == n).toList))
    groupedData.toList
  }
}
case class FoodUnit(states: List[FoodDescription]) extends Data {
  val name = states.head.name
  
  override def toString = {
    name + " {\n" +
    states.map(f => {
      val nutrients = f.nutrients.filter(n => n.nutrVal > 0)
      "\t" + f.state + (
        if (nutrients isEmpty) ""
        else " {\n" +
          nutrients.map(n => {
          val nutDef = Food.nutrientDefinitions(n.nutId)
          "\t\t" + nutDef.description + ": " + n.nutrVal + " " + nutDef.units + "\n"
          }).reduce(_ + _) +
        "\t}") + "\n"
    }).reduce(_ + _) + "},\n"
  }
}

case class FoodDescription(id: Int, description: String) extends Data {
  private val separators = Food.states.map(s => description.indexOf(s)).filter(i => i > 1).sortWith(_ < _)
  val name = if (separators isEmpty) description else description.substring(0, separators.head-2)
  val state = if (separators isEmpty) "" else description.substring(separators.head)
  var nutrients: List[NutrientData] = List()
  
  override def toString = {
    val nuts = nutrients.filter(n => n.nutrVal >= 0)
      name + (
        if (nuts isEmpty) ""
        else " {\n" +
          nuts.map(n => {
          val nutDef = Food.nutrientDefinitions(n.nutId)
          "\t" + nutDef.description + ": " + n.nutrVal + " " + nutDef.units + "\n"
          }).reduce(_ + _) +
        "}") + "\n"
  }
}

case class NutrientDefinition(nutId: Int, units: String, tagname: String, description: String) extends Data {
  override def toString = nutId + ": \"" + description + "\""
}

object NutrientData {
  def groupByFoodId(dataList: List[NutrientData]): List[List[NutrientData]] = {
    val groupedData = Queue[List[NutrientData]]()
    val dataIds = dataList.map(f => f.foodId).distinct
    
    dataIds.foreach(n => groupedData += dataList.filter(f => f.foodId == n).toList)
    groupedData.toList
  }
}
case class NutrientData(foodId: Int, nutId: Int, nutrVal: Float) extends Data