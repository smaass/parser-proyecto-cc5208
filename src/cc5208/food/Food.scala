package cc5208.food

import java.sql.Connection
import java.sql.DriverManager
import scala.collection.mutable.Queue
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.reflect.io.File

object Food {
  
  val dbURL = "jdbc:mysql://localhost/food"
  val dbUser = "root"
  val dbPass = ""
  
  def connectToDB: Try[Connection] = Try(DriverManager.getConnection(dbURL, dbUser, dbPass))
  
  def main(args: Array[String]) = {
    connectToDB match {
      case Success(connection) => {
        processData(connection)
        connection.close
      }
      case Failure(ex) => ex.printStackTrace
    }
  }
  
  val foodStates = List(
      "raw",
      "cooked",
      "boiled",
      "braised",
      "broiled",
      "simmered",
      "pan-fried",
      "grilled",
      "roasted",
      "canned",
      "frozen",
      "dehydrated",
      "oil-roasted",
      "dry-roasted",
      "dry roasted",
      "heated",
      "unheated",
      "without salt",
      "with salt")
  
  def processData(connection: Connection) = {
    val vegetables = allFoodFromGroup(connection, 1100)
    val legumes = allFoodFromGroup(connection, 1600)
    val poultry = allFoodFromGroup(connection, 500)
    val pork = allFoodFromGroup(connection, 1000)
    val beef = allFoodFromGroup(connection, 1300)
    val fish = allFoodFromGroup(connection, 1500)
    val lambVealGame = allFoodFromGroup(connection, 1700)
    
    File("vegetables.txt").writeAll(printDataGroups(groupFoodByName(vegetables)))
    File("legumes.txt").writeAll(printDataGroups(groupFoodByName(legumes)))
    File("poultry.txt").writeAll(printDataGroups(groupFoodByName(poultry)))
    File("pork.txt").writeAll(printDataGroups(groupFoodByName(pork)))
    File("beef.txt").writeAll(printDataGroups(groupFoodByName(beef)))
    File("fish.txt").writeAll(printDataGroups(groupFoodByName(fish)))
    File("lambVealGame.txt").writeAll(printDataGroups(groupFoodByName(lambVealGame)))
    
    println("Listo :)")
  }
  
  def printDataGroups(groups: List[List[Data]]): String = {
    groups.map(g => 
      "{\n" + 
      g.map(f => "\t" + printData(f) + "\n").reduce(_ + _) +
      "},\n"
    ).reduce(_ + _)
  }
  
  def nutrientDefinitions(connection: Connection): Map[Int, NutDef] = {
    val nutrients = sqlQuery(connection, "SELECT * FROM NUTR_DEF", r => {
      NutDef(r.getInt("Nutr_No"), r.getString("Units"), r.getString("Tagname"), r.getString("NutrDesc"))
    })
    nutrients.map(n => n.nutId -> n).toMap
  }
  
  def groupFoodByName(food: List[FoodDesc]): List[List[FoodDesc]] = {
    val groupedFood = Queue[List[FoodDesc]]()
    val foodNames = food.map(f => f.name).distinct
    
    foodNames.foreach(n => groupedFood += food.filter(f => f.name == n).toList)
    groupedFood.toList
  }
  
  def allFoodFromGroup(connection: Connection, group: Int): List[FoodDesc] = {
    val query = "SELECT * FROM FOOD_DES WHERE FdGrp_Cd = " + group
    
    sqlQuery(connection, query, res => {
      val longDesc = res.getString("Long_Desc")
      FoodDesc(res.getInt("NDB_No"), longDesc, foodNameFromDescription(longDesc))
    }).toList
  }
  
  def foodNameFromDescription(description: String): String = {
    val separators = foodStates.map(s => description.indexOf(s)).filter(i => i > 1).sortWith(_ < _)
    if (separators isEmpty) description else description.substring(0, separators.head)
  }
  
  def sqlQuery[A](connection: Connection, query: String, dataExtractor: (java.sql.ResultSet => A)): Queue[A] = {
    val statement = connection.createStatement
    val result = statement.executeQuery(query)
    val queue = Queue[A]()
    
    while (result.next) {
      queue += dataExtractor(result)
    }
    statement.close
    queue
  }
  
  def printData(data: Data) = data match {
    case FoodDesc(id, desc, name) => id + ": \"" + desc + "\""
  }
  
  abstract class Data
  case class FoodDesc(id: Int, description: String, name: String) extends Data
  case class NutDef(nutId: Int, units: String, tagname: String, description: String) extends Data
  case class NutData(foodId: Int, nutId: Int, nutrVal: Int) extends Data
}