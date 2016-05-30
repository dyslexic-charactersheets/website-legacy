package models

import java.io.File
import scala.io.Source


//  Iconics

case class IconicSet(filePath: String, nicePath: String) {
  val sortableName = filePath
  val (groupName, setName) = IconicImage.splitPath(nicePath)
  val id = IconicImage.slug(filePath)
  val groupDisplayName = IconicImage.withoutNumber(groupName)
  val setDisplayName = IconicImage.withoutNumber(setName)

  lazy val iconics: List[IconicImage] = IconicImage.iconics.filter(_.set == this).sortBy(_.sortableName)
}

case class IconicImage(set: IconicSet, fileName: String, niceName: String) {
  import IconicImage.slug
  val path = set.filePath+"/"+fileName
  val id = set.id+"-"+slug(fileName)
  val sortableName = id
  val largeFile = if (fileName == "") "" else "public/images/iconics/large/"+set.filePath+"/"+fileName+".png"
  val smallFile = if (fileName == "") "" else"public/images/iconics/small/"+set.filePath+"/"+fileName+".png"
  val url = if (fileName == "") "" else ("/images/iconics/small/"+set.filePath+"/"+fileName+".png").replaceAll(" ", "+")

  def copyright: Option[IconicCopyright] = {
    def cr(folder: File): Option[IconicCopyright] = {
      if (folder == null || !folder.exists() || folder.getName() == "iconics")
        None
      else {
        val copyrightFile = new File(folder.getPath+"/copyright.txt")
        if (copyrightFile.exists()) {
          println("Found copyright file: "+copyrightFile.getPath())
          val cplines = Source.fromFile(copyrightFile).getLines().toList.map(_.trim).filter(_ != "")
          cplines match {
            case cp :: link :: license :: _ => Some(IconicCopyright(cp, Some(link), Some(license)))
            case cp :: link :: _ => Some(IconicCopyright(cp, Some(link), None))
            case cp :: _ => Some(IconicCopyright(cp, None, None))
            case _ => None
          }
        } else cr(folder.getParentFile())
      }
    }

    val file = new File(largeFile)
    cr(file.getParentFile())
  }
}

case class IconicCopyright (copyright: String, url: Option[String], license: Option[String])

object IconicImage {
  lazy val iconics: List[IconicImage] = {
    val iconicsFolder = new File("public/images/iconics")
    if (!iconicsFolder.isDirectory) Nil
    else {
      val iconicsList = new File("public/images/iconics/iconics.txt")
      val lines = scala.io.Source.fromFile(iconicsList).getLines.toList
      val iconics = lines.flatMap { line =>
        try {
          val filePath :: nicePath :: _ = line.split("=").toList
          val (fileBase, fileName) = splitPath(filePath)
          val (niceBase, niceName) = splitPath(nicePath)
          val set = IconicSet(fileBase, niceBase)
          println(" - Found iconic: "+set.nicePath+" / "+niceName)
          Some(IconicImage(set, fileName, niceName))
        } catch {
          case _: Exception => None
        }
      }

      println("Found "+iconics.length+" iconics")

      iconics.sortBy(_.sortableName)
    }
  }

  val emptyIconic = IconicImage(IconicSet("", ""), "", "")

  def get(code: String): Option[IconicImage] = {
    if (code == "none") Some(emptyIconic)
    else iconics.filter(_.id == code).headOption
  }

  lazy val sets: List[IconicSet] = iconics.map(_.set).distinct.sortBy(_.sortableName)

  def withoutNumber(name: String): String = {
    val rex = """[0-9]+\s+(.*)""" r
    
    name match {
      case rex(rem) => rem
      case _ => name
    }
  }

  def sortableName(name: String): String = {
    val rex = """([0-9]+)\s+(.*)""" r
    
    name match {
      case rex(num, rem) =>
        val number = num.toInt
        ("%4d" format (number))+"-"+rem
      case _ => name
    }
  }

  def splitPath(path: String): (String, String) = {
    val reversePath: List[String] = path.split("/").toList.reverse
    val head = reversePath.head
    val tail = reversePath.tail.reverse
    val base = tail.mkString("/")
    (base, head)
  }

  def slug(str: String): String = str.toLowerCase.replaceAll("/", "--").replaceAll("[^a-z0-9/]+", "-").replaceAll("(\\.|-)png$", "").replaceAll("^-+", "").replaceAll("-+$", "")
}


//  Logos

case class LogoSet(filePath: String, nicePath: String) {
  val sortableName = IconicImage.sortableName(filePath)
  val (groupName, setName) = IconicImage.splitPath(nicePath)
  val id = IconicImage.slug(setName)
  val groupDisplayName = IconicImage.withoutNumber(groupName)
  val setDisplayName = IconicImage.withoutNumber(setName)

  lazy val logos: List[Logo] = Logo.logos.filter(_.set == this).sortBy(_.sortableName)
}

case class Logo(set: LogoSet, logoPath: String, name: String) {
  lazy val fileName: Option[String] = {
    val path = set.filePath+"/"+logoPath
    if (new File("public/images/logos/"+path+".png").exists()) Some(path+".png")
    else if (new File("public/images/logos/"+path+".jpg").exists()) Some(path+".jpg")
    else None
  }
  lazy val code = (set.filePath+"/"+logoPath).toLowerCase.replaceAll("[^a-z0-9]+", "-")
  
  def url = fileName.map("/images/logos/"+_)
  def filePath = fileName.map("public/images/logos/"+_)
  def ifExists: Option[Logo] = fileName.map(f => this)
  val sortableName = code
}

object Logo {
  lazy val logos: List[Logo] = {
    val logosFolder = new File("public/images/logos")
    if (!logosFolder.isDirectory) Nil
    else {
      val logosList = new File("public/images/logos/logos.txt")
      val lines = scala.io.Source.fromFile(logosList).getLines.toList

      val logos = lines.flatMap { line =>
        try {
          val code :: name :: _ = line.split("=").toList
          println(" - Found logo: "+code+" / "+name)

          val (fileBase, fileName) = splitPath(code)
          val (niceBase, niceName) = splitPath(name)
          val set = LogoSet(fileBase, niceBase)
          Logo(set, fileName, niceName).ifExists
        } catch {
          case _: Exception => None
        }
      }

      println("Found "+logos.length+" logos")
      logos
    }
  }

  lazy val sets: List[LogoSet] = {
    val sets = logos.map(_.set).distinct.sortBy(_.sortableName)
    for (set <- sets) {
      println("Logo set: "+set)
    }
    sets
  }

  def splitPath(path: String): (String, String) = {
    val parts: List[String] = path.split("/").toList
    var n = parts.length - 1
    if (n > 2) n = 2

    val folder = parts.take(n).mkString("/")
    val file = parts.takeRight(parts.length - n).mkString("/")
    (folder, file)
  }

  def get(code: String): Option[Logo] = logos.filter(_.code == code).headOption
  def get(codes: List[String]): Option[Logo] = codes.flatMap(code => logos.filter(_.code == code)).headOption
}
