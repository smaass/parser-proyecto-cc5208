package cc5208.food

import scala.collection.mutable.Queue

object Data {
  
  def groupByName(dataList: List[HasName]): List[List[HasName]] = {
    val groupedData = Queue[List[HasName]]()
    val dataNames = dataList.map(f => f.name).distinct
    
    dataNames.foreach(n => groupedData += dataList.filter(f => f.name == n).toList)
    groupedData.toList
  }
  
}

trait HasName {
  lazy val name: String = ""
}

abstract class Data extends HasName {
  
  override def toString = this match {
    case FoodDesc(id, desc) => id + ": \"" + desc + "\""
    case NutDef(id, units, tag, desc) => id + ": \"" + desc + "\""
  }
  
}
case class FoodDesc(id: Int, description: String) extends Data {
  override lazy val name = nameFromDescription
  
  def nameFromDescription: String = {
    val separators = Food.states.map(s => description.indexOf(s)).filter(i => i > 1).sortWith(_ < _)
    if (separators isEmpty) description else description.substring(0, separators.head)
  }
}
case class NutDef(nutId: Int, units: String, tagname: String, description: String) extends Data
case class NutData(foodId: Int, nutId: Int, nutrVal: Float) extends Data