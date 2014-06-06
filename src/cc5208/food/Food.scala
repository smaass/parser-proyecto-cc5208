package cc5208.food

object Food {
  
  val states = List(
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
  
  def nutrientDefinitions(db: DB): List[NutDef] = {
    db.sqlQuery("SELECT Nutr_No, Units, Tagname, NutrDesc FROM NUTR_DEF", r => {
      NutDef(r.getInt("Nutr_No"), r.getString("Units"), r.getString("Tagname"), r.getString("NutrDesc"))
    })
  }
  
  def allFromGroup(db: DB, group: Int): List[FoodDesc] = {
    db.sqlQuery("SELECT NDB_No, Long_Desc FROM FOOD_DES WHERE FdGrp_Cd = " + group, res => {
      val longDesc = res.getString("Long_Desc")
      FoodDesc(res.getInt("NDB_No"), longDesc)
    })
  }
  
  def allNutrientsOf(db: DB, foodId: Int): List[NutData] = {   
    db.sqlQuery("SELECT NDB_No, Nutr_No, Nutr_Val FROM NUT_DATA WHERE NDB_No=" + foodId, r => {
      NutData(r.getInt("NDB_No"), r.getInt("Nutr_No"), r.getFloat("Nutr_Val"))
    })
  }
}