package models

import java.io.File
import scala.io.Source

object StarshipData {

  def positiveData(data: Map[String, String]): List[String] = {
    val keys = data.keys.toList
    keys.filter(key => data.get(key) == Some("on"))
  }

  def parse(data: Map[String, String], gameData: GameData): StarshipData = {
    // val positive = positiveData(data)

    // data
    StarshipData(
      colour = data.get("colour").getOrElse("normal")
    )
  }
}

case class StarshipData (
  colour: String
) {
  def getGameClass(gameData: GameData): Option[GameClass] = {
    gameData.getGameClass("Starship")
  }
}