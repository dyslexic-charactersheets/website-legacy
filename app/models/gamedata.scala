package models

import java.io.File
import scala.io.Source
// import com.novus.salat._
// import com.novus.salat.global._
import play.api.Play
import play.api.libs.json._

object GameData {
  val dataPath = Play.current.configuration.getString("charactersheets.pdf.path").getOrElse("public/pdf/")+"data/"

  def load(game: String): GameData = {
    val file = new File(dataPath+game+".json")
    val data = Source.fromFile(file)("UTF-8").getLines().mkString
    val json = Json.parse(data)
    parse(json)
  }

  def parse(json: JsValue) = GameData(
      game = (json \ "game").as[String],
      name = (json \ "name").as[String],
      skills = (json \ "skills").as[List[JsObject]].map(parseSkill),
      coreSkills = (json \ "coreSkills").as[List[String]],
      summarySkills = (json \ "summarySkills").as[List[String]],
      knowledgeSkills = (json \ "knowledgeSkills").as[List[String]],
      animalSkills = (json \ "animalSkills").as[List[String]],
      consolidatedSkills = (json \ "consolidatedSkills").asOpt[Map[String, List[String]]].getOrElse(Map.empty),
      pages = (json \ "pages").as[List[JsObject]].map(parsePage),
      gm = parseGM((json \ "gm").as[JsObject]),
      base = parseBaseData((json \ "base").as[JsObject]),
      layout = (json \ "layout").as[List[List[String]]],
      books = (json \ "books").as[List[JsObject]].map(parseBook),
      languages = (json \ "languages").as[List[JsObject]].map(parseLanguageInfo),
      classes = (json \ "classes").as[List[JsObject]].map(parseBaseClass)
    )

  def parseSkill(json: JsObject) = Skill(
      name = (json \ "name").as[String],
      displayName = (json \ "displayName").asOpt[String],
      ability = (json \ "ability").asOpt[String].getOrElse(""),
      useUntrained = (json \ "useUntrained").asOpt[Boolean].getOrElse(false),
      acp = (json \ "acp").asOpt[Boolean].getOrElse(false),
      subSkillOf = (json \ "subSkillOf").asOpt[String],
      optional = (json \ "optional").asOpt[Boolean].getOrElse(false),
      afterFold = (json \ "afterFold").asOpt[Boolean].getOrElse(false),
      noRanks = (json \ "noRanks").asOpt[Boolean].getOrElse(false),
      plusLevel = (json \ "plusLevel").asOpt[Boolean].getOrElse(false),
      plusHalfLevel = (json \ "plusHalfLevel").asOpt[Boolean].getOrElse(false),
      noRage = (json \ "noRage").asOpt[Boolean].getOrElse(false),
      favouredEnemy = (json \ "favouredEnemy").asOpt[Boolean].getOrElse(false),
      favouredTerrain = (json \ "favouredTerrain").asOpt[Boolean].getOrElse(false)
    )

  def parsePage(json: JsObject) = Page(
    file = (json \ "file").as[String],
    page = (json \ "page").asOpt[Int].getOrElse(1),
    slot = (json \ "slot").asOpt[String].getOrElse(""),
    name = (json \ "name").asOpt[String].getOrElse(""),
    variant = (json \ "variant").asOpt[String],
    a5 = (json \ "a5").asOpt[Boolean].getOrElse(false),
    position = (json \ "position").asOpt[Int]
  )

  def parseGM(json: JsObject) = GM(
    characters = (json \ "characters").as[List[JsObject]].map(parsePage),
    campaign = (json \ "campaign").as[List[JsObject]].map(parsePage),
    maps = parseMaps((json \ "maps").as[JsObject]),
    kingdom = (json \ "kingdom").as[List[JsObject]].map(parsePage),
    aps = (json \ "aps").asOpt[List[JsObject]].getOrElse(Nil).map(parseAP)
  )

  def parseMaps(json: JsObject) = Maps(
    maps2d = (json \ "2d").as[List[JsObject]].map(parsePage),
    maps3d = (json \ "3d").as[List[JsObject]].map(parsePage)
  )

  def parseAP(json: JsObject) = AP(
    name = (json \ "name").as[String],
    code = (json \ "code").as[String],
    pages = (json \ "pages").as[List[JsObject]].map(parsePage)
  )

  def parseBaseData(json: JsObject) = BaseData(
    pages = (json \ "pages").as[List[String]]
  )

  def parseBook(json: JsObject) = Book(
    name = (json \ "name").as[String],
    classes = (json \ "classes").as[List[String]]   
  )

  def parseLanguageInfo(json: JsObject) = LanguageInfo(
    code = (json \ "code").as[String],
    short = (json \ "short").as[String],
    name = (json \ "name").as[String],
    ready = (json \ "ready").as[List[Float]]
  )

  def parseBaseClass(json: JsObject) = BaseClass(
    name = (json \ "name").as[String],
    pages = (json \ "pages").as[List[String]],
    variants = (json \ "variants").asOpt[List[JsObject]].getOrElse(Nil).map(parseVariant),
    axes = (json \ "axes").asOpt[List[List[String]]].getOrElse(Nil),
    skills = (json \ "skills").asOpt[List[String]].getOrElse(Nil),
    plusHalfLevel = (json \ "plusHalfLevel").asOpt[List[String]].getOrElse(Nil)
  )

  def parseVariant(json: JsObject) = VariantClass(
    name = (json \ "name").as[String],
    pages = (json \ "pages").as[List[String]],
    axes = (json \ "axes").asOpt[List[String]].getOrElse(Nil),
    skills = (json \ "skills").asOpt[List[String]].getOrElse(Nil),
    overridePlusHalfLevel = (json \ "plusHalfLevel").asOpt[List[String]],
    notSkills = (json \ "notSkills").asOpt[List[String]].getOrElse(Nil)
  )
}

case class GameData (
  game: String,
  name: String,
  skills: List[Skill],
  coreSkills: List[String],
  summarySkills: List[String],
  knowledgeSkills: List[String],
  animalSkills: List[String],
  consolidatedSkills: Map[String, List[String]],
  pages: List[Page],
  gm: GM,
  base: BaseData,
  layout: List[List[String]],
  books: List[Book],
  languages: List[LanguageInfo],
  classes: List[BaseClass]
) {
  def isPathfinder = game == "pathfinder"
  def isDnd = isDnd35
  def isDnd35 = game == "dnd35"
  def isNeoexodus = game == "neoexodus"
  def isTest = game == "test"
  def classByName(name: String) = classes.filter(_.name == name).headOption
  def bookByName(name: String) = books.filter(_.name == name).headOption

  def slugOf(str: String) = str.toLowerCase.replaceAll("[^a-z]+", " ").trim.replace(" ", "-")

  def getSkill(name: String): Option[Skill] = {
    var skill = skills.filter(_.name == name).headOption
    if (skill == None && name.startsWith("Perform")) {
      println("Making performance: "+name)
      skill = Some(Skill.makePerform(name))
    }
    if (skill == None) println(" * Unknown skill: "+name+"!")
    skill
  }

  def defaultLogo: String = game match {
    case "pathfinder" => "pathfinder-pathfinder"
    case "dnd35" => "dnd35-dnd35"
    case _ => ""
  }
}

case class GM (
  characters: List[Page],
  campaign: List[Page],
  maps: Maps,
  kingdom: List[Page],
  aps: List[AP] = Nil
  )

case class Maps (
  maps2d: List[Page],
  maps3d: List[Page]
  )

case class AP (
  name: String,
  code: String,
  pages: List[Page]
  )

case class Page (
  file: String,
  page: Int = 1,
  slot: String = "",
  name: String = "",
  variant: Option[String] = None,
  a5: Boolean,
  position: Option[Int] = None
) {
  def pagePosition = position.getOrElse(page)
}

case class BaseData (
  pages: List[String]
)

case class Book (
  name: String,
  classes: List[String]
)

trait GameClass {
  def name: String
  def shortName = name.replaceAll("^Unchained *", "").replaceAll(" *\\(.*\\)$", "")
  def pages: List[String]
  def code = name.replaceAll("[^a-zA-Z]+", "-")
  def skills: List[String]
  def plusHalfLevel: List[String]
}

case class BaseClass (
  name: String,
  pages: List[String],
  skills: List[String] = Nil,
  plusHalfLevel: List[String] = Nil,
  variants: List[VariantClass] = Nil,
  axes: List[List[String]] = Nil
) extends GameClass {
  def variantByName(name: String): Option[GameClass] = variants.filter(_.name == name).map(_.mergeInto(this)).headOption
  def axisValues: List[List[String]] = axes.zipWithIndex.map { case (axisValues,index) =>
    if (!axisValues.isEmpty) axisValues 
    else variants.map(_.axes(index)).distinct
  }
  def variantByAxes(axisValues: List[String]): Option[GameClass] = variants.filter(_.axes == axisValues).map(_.mergeInto(this)).headOption
}

case class VariantClass (
  name: String,
  pages: List[String],
  axes: List[String] = Nil,
  skills: List[String] = Nil,
  notSkills: List[String] = Nil,
  overridePlusHalfLevel: Option[List[String]] = None
) extends GameClass {
  def plusHalfLevel = overridePlusHalfLevel.getOrElse(Nil)
  def mergeInto(base: BaseClass) = new BaseClass(name, base.pages ::: pages, base.skills.filterNot(notSkills.toSet) ::: skills, overridePlusHalfLevel.getOrElse(base.plusHalfLevel))
}

case class LanguageInfo (
  code: String,
  short: String,
  name: String,
  ready: List[Float]
)

case class Skill (
  name: String,
  displayName: Option[String],
  ability: String,

  useUntrained: Boolean,
  acp: Boolean,
  subSkillOf: Option[String],
  optional: Boolean,
  afterFold: Boolean,
  noRanks: Boolean,
  plusLevel: Boolean,
  plusHalfLevel: Boolean,

  noRage: Boolean,
  favouredEnemy: Boolean,
  favouredTerrain: Boolean
) {
  def isSubSkill = subSkillOf != None
  def skillName = displayName.getOrElse(name)
}

object Skill {
  def makePerform(name: String): Skill = Skill(name, Some(name), "CHA",
    useUntrained = true,
    acp = false,
    subSkillOf = None,
    optional = false,
    afterFold = true,
    noRanks = false,
    plusLevel = false,
    plusHalfLevel = false,
    noRage = true,
    favouredEnemy = false,
    favouredTerrain = false
  )
}