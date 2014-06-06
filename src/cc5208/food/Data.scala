package cc5208.food

import scala.collection.mutable.Queue

abstract class Data {
  
  override def toString = this match {
    case FoodDescription(id, desc) => id + ": \"" + desc + "\""
    case NutrientDefinition(id, units, tag, desc) => id + ": \"" + desc + "\""
  }
}

object FoodDescription {
  
  def groupByName(dataList: List[FoodDescription]): List[List[FoodDescription]] = {
    val groupedData = Queue[List[FoodDescription]]()
    val dataNames = dataList.map(f => f.name).distinct
    
    dataNames.foreach(n => groupedData += dataList.filter(f => f.name == n).toList)
    groupedData.toList
  }
}
case class FoodDescription(id: Int, description: String) extends Data {
  lazy val name = nameFromDescription
  var nutrients: List[NutrientData] = List()
  
  def nameFromDescription: String = {
    val separators = Food.states.map(s => description.indexOf(s)).filter(i => i > 1).sortWith(_ < _)
    if (separators isEmpty) description else description.substring(0, separators.head)
  }
}

case class NutrientDefinition(nutId: Int, units: String, tagname: String, description: String) extends Data

object NutrientData {
  def groupByFoodId(dataList: List[NutrientData]): List[List[NutrientData]] = {
    val groupedData = Queue[List[NutrientData]]()
    val dataIds = dataList.map(f => f.foodId).distinct
    
    dataIds.foreach(n => groupedData += dataList.filter(f => f.foodId == n).toList)
    groupedData.toList
  }
}
case class NutrientData(foodId: Int, nutId: Int, nutrVal: Float) extends Data