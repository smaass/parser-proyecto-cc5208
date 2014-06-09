package cc5208.food

object Food {
  
  var nutrientDefinitions = Map[Int, NutrientDefinition]()
  
  val states = List(
      "raw",
      "cooked",
      "baked",
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
      
  val essentialAminoacids = List(
      "Histidine",
      "Isoleucine",
      "Leucine",
      "Lysine",
      "Methionine", 
      "Phenylalanine",
      "Threonine",
      "Tryptophan",
      "Valine")  
  
  def nutrientDefinitions(db: DB): List[NutrientDefinition] = {
    db.sqlQuery("SELECT Nutr_No, Units, Tagname, NutrDesc FROM NUTR_DEF", r => {
      NutrientDefinition(r.getInt("Nutr_No"), r.getString("Units"), r.getString("Tagname"), r.getString("NutrDesc"))
    })
  }
  
  def allFromGroup(db: DB, group: Int): List[FoodDescription] = {
    db.sqlQuery("SELECT NDB_No, Long_Desc FROM FOOD_DES WHERE FdGrp_Cd = " + group, res => {
      val longDesc = res.getString("Long_Desc")
      FoodDescription(res.getInt("NDB_No"), longDesc)
    })
  }
  
  def getNutrients(db: DB, foodIds: List[Int], nutrientIds: List[Int]) = {
    def printList(list: List[Int]): String = {
      list.head + (if (list.tail isEmpty) "" else ", " + printList(list.tail))
    }
    val query = "SELECT NDB_No, Nutr_No, Nutr_Val FROM NUT_DATA WHERE NDB_No IN (" + printList(foodIds) +
    		") AND Nutr_No IN (" + printList(nutrientIds) + ")"
    
    db.sqlQuery(query, r => {
      NutrientData(r.getInt("NDB_No"), r.getInt("Nutr_No"), r.getFloat("Nutr_Val"))
    })
  }
}