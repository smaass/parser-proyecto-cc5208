package cc5208.food

import java.sql.Connection
import java.sql.DriverManager

import scala.collection.mutable.Queue
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object DB {
  
  val dbURL = "jdbc:mysql://localhost/movedb"
  val dbUser = "root"
  val dbPass = ""
  
  def connect: Option[DB] = Try(DriverManager.getConnection(dbURL, dbUser, dbPass)) match {
    case Success(connection) => Some(new DB(connection))
    case Failure(ex) => {
      ex.printStackTrace
      None
    }
  }
}

class DB(val connection: Connection) {
  
  def sqlQuery[A](query: String, dataExtractor: (java.sql.ResultSet => A)): List[A] = {
    val statement = connection.createStatement
    val result = statement.executeQuery(query)
    val queue = Queue[A]()
    
    while (result.next) {
      queue += dataExtractor(result)
    }
    statement.close
    queue.toList
  }
  
  def close = connection.close
}