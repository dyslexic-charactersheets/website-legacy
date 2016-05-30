package models

import java.io.File
import scala.io.Source
import play.api.Play
import play.api.libs.json._

object TranslationData {
  lazy val translations = load()
  val dataPath = Play.current.configuration.getString("charactersheets.pdf.path").getOrElse("public/pdf/")+"data/"

  def apply(language: String) = translations.get(language).getOrElse(TranslationLanguage("", Nil))

  def load(): TranslationData = {
    val file = new File(dataPath+"translations.json")
    println("Loading from file: "+file.getAbsolutePath)
    val data = Source.fromFile(file)("UTF-8").getLines().mkString
    val json = Json.parse(data)
    parse(json)
  }

  def parse(json: JsValue) = TranslationData(
    translations = (json \ "languages").as[List[JsObject]].map(parseLanguage)
  )

  def parseLanguage(json: JsObject) = TranslationLanguage(
    name = (json \ "name").as[String],
    translations = (json \ "translations").as[List[JsObject]].map(parseTranslation)
    )

  def parseTranslation(json: JsObject) = Translation(
    original = (json \ "original").as[String],
    partOf = (json \ "partOf").as[Option[String]].getOrElse(""),
    translation = (json \ "translation").as[String]
    )
}

case class TranslationData (
    translations: List[TranslationLanguage]
) {
    def get(language: String): Option[TranslationLanguage] = translations.filter(_.name == language).headOption

    // def getTranslation(language: String, original: String, partOf: String): Option[Translation] = {
    //  get(language).flatMap(_.getTranslation(original, partOf))
    // }
}

case class TranslationLanguage(
    name: String,
    translations: List[Translation]
) {
    def apply(original: String): Option[String] = apply(original, "")
    def apply(original: String, partOf: String): Option[String] = {
        translations.filter(t => t.original == original && t.partOf == partOf).map(_.translation).headOption
    }
}

case class Translation (original: String, partOf: String, translation: String)