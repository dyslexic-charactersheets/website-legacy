package controllers

import play.api._
import play.api.mvc._
import play.api.data.{Form, Mapping}

import java.io.{File,FileInputStream,ByteArrayOutputStream}
import scala.io.Source
import com.itextpdf.text.pdf._
import com.itextpdf.text.{Paragraph, BaseColor, Document, Image, Element, Rectangle}

import models._
import controllers.Application.isAprilFool

object Composer extends Controller {
  lazy val pathfinderData = Application.pathfinderData
  lazy val dnd35Data = Application.dnd35Data
  lazy val testData = Application.testData

  val pdfPath: String = Play.current.configuration.getString("charactersheets.pdf.path").getOrElse("public/pdf/")

  def downloadPathfinder = downloadAction(pathfinderData)
  def downloadDnd35 = downloadAction(dnd35Data)
  def downloadTest = downloadAction(testData)

  def downloadAction(gameData: GameData) = Action(parse.multipartFormData) { request =>
    println("\n\nDownloading...")
    val customIconic = request.body.file("iconic-custom-file").map{ filepart =>
      for (contentType <- filepart.contentType)
        println("File uploaded with content type: "+contentType)
      println("File named "+filepart.filename)
      println("File at "+filepart.ref.file.getAbsolutePath)
      filepart.ref.file
    }
    val customAnimalIconic = request.body.file("animal-iconic-custom-file").map{ filepart =>
      for (contentType <- filepart.contentType)
        println("File uploaded with content type: "+contentType)
      println("File named "+filepart.filename)
      println("File at "+filepart.ref.file.getAbsolutePath)
      filepart.ref.file
    }
    val customLogo = request.body.file("logo-custom-file").map{ filepart =>
      for (contentType <- filepart.contentType)
        println("File uploaded with content type: "+contentType)
      println("File named "+filepart.filename)
      println("File at "+filepart.ref.file.getAbsolutePath)
      filepart.ref.file
    }

    val bodydata = request.body.asFormUrlEncoded
    val data: Map[String, String] = bodydata.mapValues { _.head }
    val sourceFolder = new File(pdfPath+"/"+gameData.game)

    val language = data.get("language").getOrElse("default")
    val sourceFolders = if (language != "default") {
      println("Language: "+language)
      val langFolder = new File(pdfPath+"languages/"+language+"/"+gameData.game)
      langFolder :: sourceFolder :: Nil
    } else
      sourceFolder :: Nil
    println("Source folders: "+sourceFolders.map(_.getPath).mkString(", "))

    data.get("start-type") match {
      case Some("single") =>
        println("Single...")
        val character = CharacterData.parse(data, gameData, customIconic, customAnimalIconic, customLogo)
        if (character.hasCustomIconic) println("Custom iconic found")
        if (character.hasCustomLogo) println("Custom logo found")

        val pdf = composePDF(character, gameData, sourceFolders, language)
        val filename = character.classes.toList.map(_.name).mkString(", ")+".pdf"

        Ok(pdf).as("application/pdf").withHeaders(
          "Content-disposition" -> ("attachment; filename=\""+filename+"\"")
        )

      case Some("party") =>
        val characters = CharacterData.parseParty(data, gameData, customLogo)
        val pdf = composeParty(characters, gameData, sourceFolders, language)
        val filename = characters.map(_.classes.toList.map(_.name).mkString("-")).mkString(", ")+".pdf"

        Ok(pdf).as("application/pdf").withHeaders(
          "Content-disposition" -> ("attachment; filename=\""+filename+"\"")
        )

      case Some("gm") =>
        println("Gm...")
        val gm = if(gameData.isDnd35) "Dungeon Master" else "Game Master"
        val gmPageSet = data.get("gm-start-type").getOrElse("")
        val name = gmPageSet match {
          case "characters" => "Characters and NPCs"
          case "campaign" => "Campaign Planning"
          case "maps" => "Maps"
          case "kingdom" => "Kingdom Building"
          case _ => ""
        }

        if (name == "") {
          println("Unknown GM pages: "+gmPageSet)
          NotFound
        } else {
          println("Showing "+gm+" - "+name)

          val gmdata = CharacterData.parseGM(data, gameData)
          val pdf = composeGM(gmdata, gameData, gmPageSet, sourceFolders, language)
          Ok(pdf).as("application/pdf").withHeaders(
            "Content-disposition" -> ("attachment; filename=\""+gm+" - "+name+".pdf\"")
          )
        }

      case Some("all") =>
        println("Party...")
        val character = CharacterData.parse(data, gameData, customIconic, None, customLogo)
          .copy(allKnowledge = true)
        val pdf = composeAll(character, gameData, sourceFolders, language)
        Ok(pdf).as("application/pdf").withHeaders(
          "Content-disposition" -> ("attachment; filename=\""+gameData.name+".pdf\"")
        )

      case Some(x) => 
        println(x+"?")
        NotFound

      case None =>
        NotFound
    }
  }

  def composeGM(gmdata: GMData, gameData: GameData, gmPageSet: String, folders: List[File], language: String): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val document = new Document
    val writer = PdfWriter.getInstance(document, out)
    writer.setRgbTransparencyBlending(true)
    document.open

    def placeGMPages(pages: List[Page]) {
      for (page <- pages; pageFile <- locatePage(folders, page)) {
        println("Adding page: "+page.name)
        //val pageFile = new File(folder.getPath+"/"+page.file)
        val fis = new FileInputStream(pageFile)
        val reader = new PdfReader(fis)

        println("GM page: "+page.name)

        // get the right page size
        val template = writer.getImportedPage(reader, 1)
        val pageSize = reader.getPageSize(1)
        document.setPageSize(pageSize)
        document.newPage

        //  fill with white so the blend has something to work on
        val canvas = writer.getDirectContent
        val baseLayer = new PdfLayer("Character Sheet", writer);
        canvas.beginLayer(baseLayer)
        canvas.setColorFill(BaseColor.WHITE)
        canvas.rectangle(0f, 0f, pageSize.getWidth(), pageSize.getHeight())
        canvas.fill

        canvas.setGState(defaultGstate)

        //  the page
        canvas.addTemplate(template, 0, 0)
        writeCopyright(canvas, writer, gameData, page)

        if (page.slot == "party" || page.slot == "npc-group")
          writeSkills(canvas, writer, page, gameData, None, language)

        if (page.slot == "npc")
          writeSkills(canvas, writer, page, gameData, None, language)

        writeColourOverlay(canvas, gmdata.colour, pageSize)
        canvas.endLayer()

        writeLogo(canvas, writer, page.slot, gameData, None, Some(gmdata))

        //  done
        fis.close
      }
    }

    val pages: List[Page] = gmPageSet match {
      case "characters" => 
        val pages = gameData.gm.characters
        println("Looking for party sheet for "+Some(gmdata.numPCs)+" PCs")
        val party = pages.filter{p => println(" - "+p.slot+" / "+p.variant); p.slot == "party" && p.variant == Some(gmdata.numPCs.toString)}.headOption
        println(" - Found: "+party)
        val otherPages = pages.filter(p => p.slot != "party")
        party.toList ::: otherPages
      case "campaign" => gameData.gm.campaign
      case "maps" => 
        if (gmdata.maps3d) gameData.gm.maps.maps3d else gameData.gm.maps.maps2d
      case "kingdom" => 
        val kingdom = gameData.gm.kingdom.filter(_.slot == "kingdom")
        println("Looking for settlement: "+gmdata.settlementStyle+"; "+gameData.gm.kingdom.map(p => p.slot+"/"+p.variant).mkString(", "))
        val settlement = gameData.gm.kingdom.filter(p => p.slot == "settlement" && p.variant == Some(gmdata.settlementStyle))
        kingdom ::: settlement ::: Nil
      case _ => 
        println("Unknown GM pages: "+gmPageSet)
        Nil
    }
    placeGMPages(pages)

    // if (gmdata.maps) {
    //   val maps = if (gmdata.maps3d) gameData.gm.maps.maps3d else gameData.gm.maps.maps2d
    //   placeGMPages(maps)
    // }
    // if (gmdata.gmCampaign)
    //   placeGMPages(gameData.gm.campaign)
    // println("APs: "+gmdata.aps.mkString(", "))
    // for (ap <- gameData.gm.aps) {
    //   if (gmdata.aps.contains(ap.code))
    //     placeGMPages(ap.pages)
    // }
    
    document.close
    out.toByteArray
  }

  def composeParty(characters: List[CharacterData], gameData: GameData, folders: List[File], language: String): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val document = new Document
    val writer = PdfWriter.getInstance(document, out)
    writer.setRgbTransparencyBlending(true)
    document.open

    for (character <- characters) {
      println("START OF CHARACTER")
      addCharacterPages(character, gameData, folders, document, writer, language)
    }

    document.close
    out.toByteArray
  }

  def composePDF(character: CharacterData, gameData: GameData, folders: List[File], language: String): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val document = new Document
    val writer = PdfWriter.getInstance(document, out)
    writer.setRgbTransparencyBlending(true)
    document.open

    addCharacterPages(character, gameData, folders, document, writer, language)

    document.close
    out.toByteArray
  }

  def locatePage(folders: List[File], page: Page): Option[File] = locatePageFile(folders, page.file)

  def locatePageFile(folders: List[File], filename: String): Option[File] = {
    val availableFiles = folders.map(folder => new File(folder.getPath+"/"+filename)).filter(_.exists)
    println("Locate file: "+filename+": "+availableFiles.map(_.getPath).mkString(", "))
    availableFiles.headOption
  }

  def defaultGstate: PdfGState = {
    val defaultGstate = new PdfGState
    defaultGstate.setBlendMode(PdfGState.BM_NORMAL)
    defaultGstate.setFillOpacity(1.0f)
    defaultGstate
  }

  val aprilFoolIconic = IconicImage(IconicSet("1-paizo/4-advanced-races", "1 Paizo/4 Advanced Races"), "goblin-d20", "Goblin - d20")

  def addCharacterPages(character: CharacterData, gameData: GameData, folders: List[File], document: Document, writer: PdfWriter, language: String) {
    val pages = new CharacterInterpretation(gameData, character).pages

    val colour = character.colour
    for (page <- pages; pageFile <- locatePage(folders, page)) {
      //val pageFile = new File(folder.getPath+"/"+page.file)
      val fis = new FileInputStream(pageFile)
      val reader = new PdfReader(fis)

      // get the right page size
      val template = writer.getImportedPage(reader, 1)
      val pageSize = reader.getPageSize(1)
      document.setPageSize(pageSize)
      document.newPage

      //  fill with white so the blend has something to work on
      val canvas = writer.getDirectContent
      val baseLayer = new PdfLayer("Character Sheet", writer);
      canvas.beginLayer(baseLayer)
      canvas.setColorFill(BaseColor.WHITE)
      canvas.rectangle(0f, 0f, pageSize.getWidth(), pageSize.getHeight())
      canvas.fill


      //  the page
      canvas.addTemplate(template, 0, 0)

      //  copyright notice
      writeCopyright(canvas, writer, gameData, page)

      //  generic image
      if (!character.hasCustomIconic && !character.iconic.isDefined && !isAprilFool)
        writeIconic(canvas, writer, page.slot, "public/images/iconics/generic.png", None, character)

      // skills
      if (page.slot == "core")
        writeSkills(canvas, writer, page, gameData, Some(character), language)
      if (page.slot == "eidolon")
        writeSkills(canvas, writer, page, gameData, Some(character.makeEidolon(gameData)), language)
      if (page.slot == "animalcompanion")
        writeSkills(canvas, writer, page, gameData, Some(character.makeAnimalCompanion(gameData)), language)

      // variant rules
      if (!character.variantRules.isEmpty) {
        if (character.variantRules.contains("wounds-vigour")) {
          overlayPage(canvas, writer, folders, "Pathfinder/Variant Rules/Wounds and Vigour.pdf")
        }
      }

      // april fool
      if (isAprilFool) {
        page.slot match {
          case "core" => overlayPage(canvas, writer, folders, "Extra/Special Overlays/Character Info.pdf")
          case "inventory" => overlayPage(canvas, writer, folders, "Extra/Special Overlays/Inventory.pdf")
          case _ => 
        }
      }

      writeColourOverlay(canvas, colour, pageSize)

      canvas.endLayer()

      //  logo
      writeLogo(canvas, writer, page.slot, gameData, Some(character), None)

      //  iconics
      if (character.hasCustomIconic)
        writeIconic(canvas, writer, page.slot, character.customIconic.get.getAbsolutePath, None, character)
      else if (character.iconic.isDefined)
        writeIconic(canvas, writer, page.slot, character.iconic.get.largeFile, character.iconic, character)
      else if (isAprilFool)
        writeIconic(canvas, writer, page.slot, aprilFoolIconic.largeFile, None, character)

      //  watermark
      if (character.watermark != "") {
        writeWatermark(canvas, writer, character.watermark, colour, pageSize)
      }

      fis.close
    }
  }

  def overlayPage(canvas: PdfContentByte, writer: PdfWriter, folders: List[File], fileName: String) {
    for (pageFile <- locatePageFile(folders, fileName)) {
      // val pageFile = new File(folder.getPath+"/"+fileName)
      val fis = new FileInputStream(pageFile)
      val reader = new PdfReader(fis)
      val template = writer.getImportedPage(reader, 1)

      canvas.setGState(defaultGstate)

      //  the page
      canvas.addTemplate(template, 0, 0)
    }
  }

  def writeCopyright(canvas: PdfContentByte, writer: PdfWriter, gameData: GameData, page: Page) {
    val year = new org.joda.time.LocalDate().getYear()

    //  copyright notice
    canvas.setColorFill(new BaseColor(0.5f, 0.5f, 0.5f))
    val font = textFont

    canvas.beginText
    val copyrightLayer = new PdfLayer("Iconic image", writer)
    canvas.beginLayer(copyrightLayer)
    canvas.setFontAndSize(font, 5.5f)
    canvas.showTextAligned(Element.ALIGN_LEFT, "\u00A9 Marcus Downing "+year+"        http://charactersheets.minotaur.cc", 30, 22, 0)
    canvas.setFontAndSize(font, 4.5f)

    if (page.a5) {
      if (gameData.isPathfinder) {
        canvas.showTextAligned(Element.ALIGN_LEFT, "This character sheet uses trademarks and/or copyrights owned by Paizo Publishing, LLC, which are used under Paizo's", 180, 22, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "Community Use Policy. We are expressly prohibited from charging you to use or access this content. This character sheet is not published, endorsed, or specifically approved by Paizo Publishing.", 30, 17, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "For more information about Paizo's Community Use Policy, please visit paizo.com/communityuse. For more information about Paizo Publishing and Paizo products, please visit paizo.com.", 30, 12, 0)
      } else {
        canvas.showTextAligned(Element.ALIGN_LEFT, "This character sheet is not affiliated with, endorsed, sponsored, or specifically approved by Wizards of the Coast LLC. ", 180, 22, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "This character sheet may use the trademarks and other intellectual property of Wizards of the Coast LLC, which is permitted under Wizards' Fan Site Policy. For example, DUNGEONS & DRAGONS®, D&D®,", 30, 17, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "PLAYER'S HANDBOOK 2®, and DUNGEON MASTER'S GUIDE® are trademark[s] of Wizards of the Coast and D&D® core rules, game mechanics, characters and their distinctive likenesses are the property of", 30, 12, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "the Wizards of the Coast. For more information about Wizards of the Coast or any of Wizards' trademarks or other intellectual property, please visit their website.", 30, 7, 0)
      }
    } else {
      if (gameData.isPathfinder) {
        canvas.showTextAligned(Element.ALIGN_LEFT, "This character sheet uses trademarks and/or copyrights owned by Paizo Publishing, LLC, which are used under Paizo's Community Use Policy. We are expressly prohibited from charging you to use or access this content.", 180, 22, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "This character sheet is not published, endorsed, or specifically approved by Paizo Publishing. For more information about Paizo's Community Use Policy, please visit paizo.com/communityuse. For more information about Paizo Publishing and Paizo products, please visit paizo.com.", 30, 17, 0)
      } else if (gameData.isDnd35) {
        canvas.showTextAligned(Element.ALIGN_LEFT, "This character sheet is not affiliated with, endorsed, sponsored, or specifically approved by Wizards of the Coast LLC. This character sheet may use the trademarks and other intellectual property of", 180, 22, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "Wizards of the Coast LLC, which is permitted under Wizards' Fan Site Policy. For example, DUNGEONS & DRAGONS®, D&D®, PLAYER'S HANDBOOK 2®, and DUNGEON MASTER'S GUIDE® are trademark[s] of Wizards of the Coast and D&D® core rules, game mechanics, characters and their", 30, 17, 0)
        canvas.showTextAligned(Element.ALIGN_LEFT, "distinctive likenesses are the property of the Wizards of the Coast. For more information about Wizards of the Coast or any of Wizards' trademarks or other intellectual property, please visit their website.", 30, 12, 0)
      }
    }
    canvas.endLayer
    canvas.endText
  }

  //  1mm = 2.8pt
  def pickSkills(page: Page, gameData: GameData, character: Option[CharacterData], translate: TranslationLanguage): (List[Skill], List[String], Map[String, List[Skill]]) = {
    val skillsStyle = character.map(_.skillsStyle).getOrElse("normal")
    if (skillsStyle == "blank") return (Nil, Nil, Map.empty)

    var classSkills: List[String] = character.map(_.classes.flatMap(_.skills).distinct).getOrElse(Nil)
    val bonusSkills = if (isAprilFool) "Knowledge (aeronautics)" :: Nil else Nil

    var skills: List[Skill] = page.slot match {
      case "party" =>
        gameData.summarySkills.flatMap(gameData.getSkill)

      case "npc-group" =>
        gameData.summarySkills.flatMap(gameData.getSkill)

      case "npc" =>
        gameData.coreSkills.flatMap(gameData.getSkill)

      case "animalcompanion" =>
        gameData.animalSkills.flatMap(gameData.getSkill)

      case "eidolon" =>
        val eidolonClass = gameData.classByName("Eidolon")
        val eidolonSkills: List[String] = eidolonClass.toList.flatMap(_.skills)
        println("Eidolon skills: "+eidolonSkills.mkString(", "))
        classSkills = eidolonSkills
        (gameData.coreSkills ::: eidolonSkills ::: bonusSkills).distinct.flatMap(gameData.getSkill)

      case _ =>
        val knowledgeSkills = if (character.map(_.allKnowledge).getOrElse(false)) gameData.knowledgeSkills else Nil
        val performSkill = character.map(_.performSkill.toList).getOrElse(Nil)
        // println("Core skills: "+coreSkills.mkString(", "))
        // println("Class skills: "+classSkills.mkString(", "))
        if (knowledgeSkills != Nil) println("Knowledge skills: "+knowledgeSkills.mkString(", "))
        if (bonusSkills != Nil) println("Bonus skills: "+bonusSkills.mkString(", "))

        (gameData.coreSkills ::: classSkills ::: knowledgeSkills ::: performSkill ::: bonusSkills).distinct.flatMap(gameData.getSkill)
    }

    if (skillsStyle == "consolidated" && !gameData.consolidatedSkills.isEmpty) {
      skills = skills.flatMap { skill =>
        if (gameData.consolidatedSkills.contains(skill.name)) {
          gameData.consolidatedSkills(skill.name).flatMap{ as => 
            gameData.skills.filter(_.name == as)
          }
        } else {
          skill :: Nil
        }
      }.map { skill =>
        if (skill.subSkillOf.isDefined) {
          skill.subSkillOf match {
            case Some(parent) if gameData.consolidatedSkills.contains(parent) =>
              val as = gameData.consolidatedSkills(parent).head
              skill.copy(subSkillOf = Some(as))
            case _ => skill
          }
        } else skill
      }.distinct
    }

    // translate skill names before sorting them
    skills = skills.map { s => 
      val name = s.skillName
      s.copy(displayName = Some(translate(name).getOrElse(name)))
    }

    // println("Writing skills: "+skills.map(_.name))
    val (afterFold, beforeFold) = skills.filter(!_.isSubSkill).partition(_.afterFold)
    val topSkills = beforeFold.sortBy(_.skillName) ::: afterFold.sortBy(_.skillName)

    val subskillsBySkill: Map[String, List[Skill]] = skills.filter(_.isSubSkill).groupBy(_.subSkillOf.getOrElse(""))

    (topSkills, classSkills, subskillsBySkill)
  }

  def writeSkills(canvas: PdfContentByte, writer: PdfWriter, page: Page,  gameData: GameData, character: Option[CharacterData], language: String) {
    val translate = TranslationData(language)
    val (skillsToWrite, classSkills, subskillsBySkill) = pickSkills(page, gameData, character, translate)

    //  set values up
    val skillFont = textFont
    val skillFontSize = 8f
    val attrFont = altFont
    val attrFontSize = 10.4f
    val xFont = barbarianFont2
    val stdColour = new BaseColor(0.4f, 0.4f, 0.4f)
    val fillColour = new BaseColor(0.6f, 0.6f, 0.6f)
    val attrColour = stdColour
    val white = new BaseColor(1f, 1f, 1f)
    val black = new BaseColor(0f, 0f, 0f)
    val fadedGState = new PdfGState
    fadedGState.setBlendMode(PdfGState.BM_NORMAL)
    fadedGState.setFillOpacity(0.3f)

    val skillLayout = getSkillLayout(page, gameData)
    import skillLayout._

    def writeCheckbox(x: Float, y: Float, filled: Boolean) {
      val radius = 2.4f

      if (filled) {
        canvas.setColorFill(fillColour)
      } else {
        canvas.setColorFill(white)
      }
      canvas.setGState(defaultGstate)
      canvas.rectangle(x, y, radius * 2f, radius * 2f)
      canvas.fill()

      canvas.setColorStroke(stdColour)
      canvas.setGState(defaultGstate)
      canvas.setLineWidth(0.5f)
      canvas.rectangle(x, y, radius * 2f, radius * 2f)
      canvas.stroke()
    }

    def writeSkillLine(pos: Int, skill: Skill, isSubSkill: Boolean) {
      val y = firstLine + pos * lineIncrement
      val nameLeft = if (isSubSkill) skillNameLeft + skillNameIndent else skillNameLeft
      canvas.setFontAndSize(skillFont, skillFontSize)
      canvas.setColorFill(stdColour)
      canvas.setGState(defaultGstate)
      canvas.beginText
      canvas.showTextAligned(Element.ALIGN_LEFT, skill.skillName, nameLeft, y, 0)
      canvas.endText

      if (isSubSkill) {
        val top = firstLine + (pos - 1) * lineIncrement + lineBottomOffset
        val bottom = firstLine + pos * lineIncrement + 2
        val midv = (top + top + bottom) / 3f

        val left = skillsAreaLeft + 7
        val right = left + (top - bottom)
        val midh = left + (top - bottom) * 2f / 3f

        canvas.setColorStroke(stdColour)
        canvas.setGState(defaultGstate)
        canvas.setLineWidth(0.5f)

        canvas.moveTo(left, top)
        canvas.lineTo(left, midv)
        canvas.curveTo(left, bottom, left, bottom, midh, bottom)
        // canvas.lineTo(midh, bottom)
        canvas.lineTo(right, bottom)
        canvas.stroke()
      }

      val isCorePage = page.slot == "core" || page.slot == "eidolon"
      if (isCorePage) {
        if (skill.useUntrained) {
          writeCheckbox(useUntrainedMiddle, y, true)
        }

        val ability = if (skill.ability.length > 0) translate(skill.ability).getOrElse(skill.ability) else ""
        canvas.setFontAndSize(attrFont, attrFontSize)
        canvas.setColorFill(attrColour)
        canvas.setGState(fadedGState)
        canvas.beginText
        canvas.showTextAligned(Element.ALIGN_CENTER, ability, abilityMiddle, y + abilityOffset, 0)
        canvas.endText

        canvas.setGState(defaultGstate)

        if (!isSubSkill && !skill.noRanks) {
          if (gameData.isPathfinder) {
            writeCheckbox(classSkillMiddle, y, classSkills.contains(skill.name))
          } else if (gameData.isDnd35) {
            val nmax = if (page.variant == Some("more")) 7 else 5
            for (i <- Range(0, nmax)) {
              writeCheckbox(classSkillMiddle + classSkillIncrement * i, y, false)
            }

            for ( char <- character; (cls, i) <- char.classes.zipWithIndex) {
              if (i < nmax)
                writeCheckbox(classSkillMiddle + classSkillIncrement * i, y, cls.skills.contains(skill.name))
            }
          }
        }

        // work out if the skill gets level bonuses
        var plusHalfLevelClasses: List[GameClass] = Nil
        var plusLevelClasses: List[GameClass] = Nil

        if (isSubSkill || skill.noRanks) {
          if (skill.plusLevel) {
            plusLevelClasses = character.toList.flatMap(_.classes).filter(_.skills.contains(skill.name))
          } else if (skill.plusHalfLevel) {
            plusHalfLevelClasses = character.toList.flatMap(_.classes).filter(_.skills.contains(skill.name))
          }
        }

        for ( char <- character.toList; cls <- char.classes; if cls.plusHalfLevel.contains(skill.name) ) {
          plusHalfLevelClasses = cls :: plusHalfLevelClasses
        }

        var (plusLevelPlusX, plusLevelX) = 
          if (isSubSkill) (abilityMiddle - 6f, abilityMiddle + 14f)
          else if (skill.noRanks) (classSkillMiddle + 2.5f, ranksMiddle - 1.5f)
          else (ranksMiddle + 12f, ranksMiddle + 32f)

        // write level bonuses
        // println(skill.skillName+" plus half level classes: "+plusHalfLevelClasses.map(_.name).mkString(", "))
        // println(skill.skillName+" plus level classes: "+plusHalfLevelClasses.map(_.name).mkString(", "))
        if (!plusLevelClasses.isEmpty || !plusHalfLevelClasses.isEmpty) {
          canvas.setFontAndSize(attrFont, attrFontSize)
          canvas.setColorFill(stdColour)
          canvas.setGState(defaultGstate)
          canvas.beginText
          canvas.showTextAligned(Element.ALIGN_CENTER, "+", plusLevelPlusX, y - 2f, 0)
          canvas.endText

          if (!plusHalfLevelClasses.isEmpty) {
            plusLevelX -= 12f
            canvas.setFontAndSize(textFont, attrFontSize)
            canvas.beginText
            canvas.showTextAligned(Element.ALIGN_CENTER, "(", plusLevelX, y - 2f, 0)
            canvas.endText
            plusLevelX += 17f

            canvas.setFontAndSize(skillFont, 6f)
            canvas.setColorFill(attrColour)
            canvas.setGState(fadedGState)
            val level = translate("Level").getOrElse("Level")
            for ( cls <- plusHalfLevelClasses ) {
              val className = translate(cls.shortName).getOrElse(cls.shortName)
              canvas.beginText
              canvas.showTextAligned(Element.ALIGN_CENTER, className, plusLevelX, y + 2.5f, 0)
              canvas.showTextAligned(Element.ALIGN_CENTER, level, plusLevelX, y - 2.5f, 0)
              canvas.endText
              plusLevelX += 26f
            }

            plusLevelX -= 2f
            canvas.setFontAndSize(attrFont, attrFontSize)
            canvas.setColorFill(stdColour)
            canvas.setGState(defaultGstate)
            canvas.beginText
            canvas.showTextAligned(Element.ALIGN_CENTER, "÷ 2", plusLevelX, y - 2f, 0)
            canvas.endText
            plusLevelX += 11f

            canvas.setFontAndSize(textFont, attrFontSize)
            canvas.beginText
            canvas.showTextAligned(Element.ALIGN_CENTER, ")", plusLevelX, y - 2f, 0)
            canvas.endText
            plusLevelX += 20f
          }

          if (!plusLevelClasses.isEmpty) {
            canvas.setFontAndSize(skillFont, 6f)
            canvas.setColorFill(attrColour)
            canvas.setGState(fadedGState)
            val level = translate("Level").getOrElse("Level")
            for ( cls <- plusLevelClasses ) {
              val className = translate(cls.shortName).getOrElse(cls.shortName)
              canvas.beginText
              canvas.showTextAligned(Element.ALIGN_CENTER, className, plusLevelX, y + 2.5f, 0)
              canvas.showTextAligned(Element.ALIGN_CENTER, level, plusLevelX, y - 2.5f, 0)
              canvas.endText
              plusLevelX += 26f
            }
          }
        } else if (isSubSkill || skill.noRanks) {
          canvas.setFontAndSize(attrFont, attrFontSize - 2)
          canvas.setColorFill(attrColour)
          canvas.setGState(fadedGState)
          canvas.beginText
          canvas.showTextAligned(Element.ALIGN_CENTER, "/", ranksMiddle, y + abilityOffset / 2, 0)
          canvas.endText
          canvas.setGState(defaultGstate)
        }

        if (page.slot == "core" && skill.acp) {
          canvas.setColorFill(stdColour)
          canvas.setFontAndSize(attrFont, attrFontSize)
          canvas.beginText
          canvas.showTextAligned(Element.ALIGN_CENTER, "-", skillsAreaRight - acpWidth - 5, y - 1, 0)
          canvas.endText

          canvas.setColorStroke(stdColour)
          val isAcpDouble = gameData.isDnd35 && skill.name == "Swim"
          canvas.setLineWidth(if (isAcpDouble) 1.5f else 0.5f)
          canvas.setLineDash(2f, 2f)
          canvas.rectangle(skillsAreaRight - acpWidth, y + lineBottomOffset, acpWidth, lineBoxHeight)
          canvas.stroke()

          canvas.setLineDash(0f)

          if (isAcpDouble) {
            canvas.setFontAndSize(attrFont, attrFontSize - 2)
            canvas.setColorFill(attrColour)
            canvas.setGState(fadedGState)
            canvas.beginText
            canvas.showTextAligned(Element.ALIGN_CENTER, "x 2", skillsAreaRight - acpWidth / 2, y + abilityOffset / 2, 0)
            canvas.endText
            canvas.setGState(defaultGstate)
          }
        }

        if (page.variant == Some("barbarian") && skill.noRage) {
          canvas.setColorFill(black)
          canvas.setFontAndSize(xFont, skillFontSize)
          canvas.beginText
          canvas.showTextAligned(Element.ALIGN_CENTER, "X", rageMiddle, y - 1, 0)
          canvas.endText
        }

        if (page.variant == Some("ranger")) {
          if (skill.favouredEnemy) {
            canvas.setColorFill(stdColour)
            // canvas.rectangle(favouredEnemyMiddle - 2, y, 4, 4)
            canvas.moveTo(favouredEnemyMiddle - 2f, y)
            canvas.curveTo(favouredEnemyMiddle - 1.5f, y + 1f, favouredEnemyMiddle - 1.5f, y + 3f, favouredEnemyMiddle - 2f, y + 4f)
            canvas.curveTo(favouredEnemyMiddle - 1f, y + 3.5f, favouredEnemyMiddle + 1f, y + 3.5f, favouredEnemyMiddle + 2f, y + 4f)
            canvas.curveTo(favouredEnemyMiddle + 1.5f, y + 3f, favouredEnemyMiddle + 1.5f, y + 1f, favouredEnemyMiddle + 2f, y)
            canvas.curveTo(favouredEnemyMiddle + 1f, y + 0.5f, favouredEnemyMiddle - 1f, y + 0.5f, favouredEnemyMiddle - 2f, y)
            canvas.fill()
          }
        }

        if (page.variant == Some("ranger") || page.variant == Some("worldwalker")) {
          if (skill.favouredTerrain) {
            canvas.setColorStroke(stdColour)
            canvas.circle(favouredEnemyMiddle, y + 2, 4.5f)
            canvas.setLineWidth(1.2f)
            canvas.stroke()

          }
        }

        def annotateSkill(sigil: String, line1: String, line2: String) {
          val sigilr =
            if (skill.acp) {
              if (skill.noRage && page.variant == Some("barbarian"))
                   skillsAreaRight - acpWidth * 3
              else skillsAreaRight - acpWidth * 2
            } else skillsAreaRight - acpWidth + 3f
          val linesl = sigilr + 2f

          canvas.beginText
          canvas.setColorFill(stdColour)
          canvas.setFontAndSize(skillFont, 8f)
          canvas.showTextAligned(Element.ALIGN_RIGHT, sigil, sigilr, y - 1f, 0)

          canvas.setFontAndSize(skillFont, 4.5f)
          canvas.showTextAligned(Element.ALIGN_LEFT, line1, linesl, y + 3f, 0)
          canvas.showTextAligned(Element.ALIGN_LEFT, line2, linesl, y - 2f, 0)
          canvas.endText
        }

        if (gameData.isPathfinder) {
          if (skill.name == "Intimidate") {
            annotateSkill("±4", "if larger/", "smaller")
            // canvas.setColorFill(stdColour)
            // canvas.setFontAndSize(skillFont, 8f)
            // canvas.showTextAligned(Element.ALIGN_RIGHT, "±4", skillsAreaRight - acpWidth + 4f, y - 2f, 0)

            // canvas.setFontAndSize(skillFont, 4.5f)
            // canvas.showTextAligned(Element.ALIGN_LEFT, "if larger/", skillsAreaRight - acpWidth + 7f, y + 3f, 0)
            // canvas.showTextAligned(Element.ALIGN_LEFT, "smaller", skillsAreaRight - acpWidth + 7f, y - 2f, 0)
          }
        } else if (gameData.isDnd35) {
          if (skill.name == "Intimidate") annotateSkill("+", "size", "diff x4")
          if (skill.name == "Hide") annotateSkill("+", "size", "mod x4")
          if (skill.name == "Swim") annotateSkill("-1", "per 5lb", "carried")
        }
      }
    }

    // write it
    var pos = 0
    var foldWritten = false
    for (skill <- skillsToWrite) {

      // draw the fold
      if (skill.afterFold && !foldWritten) {
        val liney = firstLine + (pos - 1) * lineIncrement + lineBottomOffset
        canvas.setColorFill(stdColour)
        canvas.setGState(defaultGstate)
        canvas.setLineWidth(1f)
        canvas.moveTo(skillsAreaLeft, liney)
        canvas.lineTo(skillsAreaRight, liney)
        canvas.stroke()
        foldWritten = true
      }

      writeSkillLine(pos, skill, false)
      pos = pos + 1

      //  subskills
      if (subskillsBySkill.contains(skill.name)) {
        for (subskill <- subskillsBySkill(skill.name)) {
          writeSkillLine(pos, subskill, true)
          pos = pos + 1
        }
      }
    }

    if (page.slot == "core") {
      while (pos < numSlots) {
        val y = firstLine + pos * lineIncrement
        writeCheckbox(useUntrainedMiddle, y, false)
        writeCheckbox(classSkillMiddle, y, false)
        pos = pos + 1
      }
    }

  }

  case class SkillLayout (firstLine: Float, lineIncrement: Float, lineBottomOffset: Float, lineBoxHeight: Float, skillsAreaLeft: Float, skillsAreaRight: Float, 
    skillNameLeft: Float, skillNameIndent: Float, abilityMiddle: Float, abilityOffset: Float, ranksMiddle: Float,
    useUntrainedMiddle: Float, classSkillMiddle: Float, classSkillIncrement: Float, acpWidth: Float, numSlots: Int,
    rageMiddle: Float, favouredEnemyMiddle: Float)
  def getSkillLayout(page: Page, gameData: GameData): SkillLayout = page.slot match {
    case "party" =>
      val firstLine = 496f
      val lineIncrement = -13.55f
      val skillsAreaLeft = 28f
      val skillsAreaRight = if (page.variant == "10") 39.5f else 40f
      val lineBottomOffset = -4.5f

      val skillNameLeft = skillsAreaLeft + 2f
      val skillNameIndent = 16f

      val numSlots = 0

      SkillLayout(firstLine, lineIncrement, lineBottomOffset, 0, skillsAreaLeft, skillsAreaRight, 
        skillNameLeft, skillNameIndent, 0, 0, 0, 0, 0, 0, 0, numSlots, 0, 0)

    case "animalcompanion" =>
      val firstLine = if (gameData.isPathfinder) 469f else 445f
      val lineIncrement = -15f
      val skillsAreaLeft = 189f
      val skillsAreaRight = 300f
      val lineBottomOffset = -4.5f

      val skillNameLeft = skillsAreaLeft + 2f
      val skillNameIndent = 16f

      val numSlots = 0

      SkillLayout(firstLine, lineIncrement, lineBottomOffset, 0, skillsAreaLeft, skillsAreaRight, 
        skillNameLeft, skillNameIndent, 0, 0, 0, 0, 0, 0, 0, numSlots, 0, 0)

    case "npc" =>
      val firstLine = 512f
      val lineIncrement = -13.7f
      val skillsAreaLeft = 189f
      val skillsAreaRight = 300f
      val lineBottomOffset = -4.5f

      val skillNameLeft = skillsAreaLeft + 2f
      val skillNameIndent = 16f

      val numSlots = 0

      SkillLayout(firstLine, lineIncrement, lineBottomOffset, 0, skillsAreaLeft, skillsAreaRight, 
        skillNameLeft, skillNameIndent, 0, 0, 0, 0, 0, 0, 0, numSlots, 0, 0)

    case "npc-group" =>
      val firstLine = 452.25f
      val lineIncrement = -13.55f
      val skillsAreaLeft = 28f
      val skillsAreaRight = 40f
      val lineBottomOffset = -4.5f

      val skillNameLeft = skillsAreaLeft + 2f
      val skillNameIndent = 16f

      val numSlots = 0

      SkillLayout(firstLine, lineIncrement, lineBottomOffset, 0, skillsAreaLeft, skillsAreaRight, 
        skillNameLeft, skillNameIndent, 0, 0, 0, 0, 0, 0, 0, numSlots, 0, 0)

    case "eidolon" =>
      val firstLine = 616f
      val lineIncrement = -13.51f
      val lineBottomOffset = -4.5f
      val lineBoxHeight = 13f

      val skillsAreaLeft = 232f
      val skillsAreaRight = 567f

      val skillNameLeft = skillsAreaLeft + 2f
      val skillNameIndent = 16f
      val abilityMiddle = 420f
      val abilityOffset = -1f

      val ranksMiddle = 465f

      val useUntrainedMiddle = 366f
      val classSkillMiddle = 445f
      val classSkillIncrement = 10f

      val rageMiddle = 524f
      val favouredEnemyMiddle = 524f
      val acpWidth = 25f

      val numSlots = 43

      SkillLayout(firstLine, lineIncrement, lineBottomOffset, lineBoxHeight, skillsAreaLeft, skillsAreaRight, 
        skillNameLeft, skillNameIndent, abilityMiddle, abilityOffset, ranksMiddle,
        useUntrainedMiddle, classSkillMiddle, classSkillIncrement, acpWidth, numSlots,
        rageMiddle, favouredEnemyMiddle)

    case _ if gameData.isPathfinder =>
      println("Pathfinder skill points for page variant: "+page.slot+" / "+page.variant)
      val isBarbarian = page.variant == Some("barbarian")
      val isRanger = page.variant == Some("ranger")

      val firstLine = 603f
      val lineIncrement = -13.51f
      val lineBottomOffset = -4.5f
      val lineBoxHeight = 13f

      val skillsAreaLeft = 231f
      val skillsAreaRight = 567f

      val skillNameLeft = skillsAreaLeft + 2f
      val skillNameIndent = 16f
      val abilityMiddle = 
        if (isBarbarian) 395f 
        else if (isRanger) 388f
        else 410f
      val abilityOffset = -1f

      val ranksMiddle = 
        if (isBarbarian) 448f
        else if (isRanger) 443f
        else 465f

      val useUntrainedMiddle = 
        if (isBarbarian) 341f
        else if (isRanger) 335f
        else 356f
      val classSkillMiddle = 
        if (isBarbarian) 419f
        else if (isRanger) 413f
        else 435f
      val classSkillIncrement = 10f

      val rageMiddle = 524f
      val favouredEnemyMiddle = 524f
      val acpWidth = 25f

      val numSlots = 43

      SkillLayout(firstLine, lineIncrement, lineBottomOffset, lineBoxHeight, skillsAreaLeft, skillsAreaRight, 
        skillNameLeft, skillNameIndent, abilityMiddle, abilityOffset, ranksMiddle,
        useUntrainedMiddle, classSkillMiddle, classSkillIncrement, acpWidth, numSlots,
        rageMiddle, favouredEnemyMiddle)
    

    case _ if gameData.isDnd35 =>
      println("D&D 3.5 skill points for page variant: "+page.slot+" / "+page.variant)
      val isBarbarian = page.variant == Some("barbarian")
      val isRanger = page.variant == Some("ranger")
      val isMore = page.variant == Some("more")
      val isSimple = page.variant == Some("simple")

      val firstLine: Float = 
        if (isSimple) 615f
        else if (isMore) 588f 
        else 616f
      val lineIncrement = -13.51f
      val lineBottomOffset = -4.5f
      val lineBoxHeight = 13f

      val skillsAreaLeft = 231f
      val skillsAreaRight = 567f

      val skillNameLeft = skillsAreaLeft + 2f
      val skillNameIndent = 16f
      val abilityMiddle =
        if (isMore) 370f
        else 383f
      val abilityOffset = -1f

      val ranksMiddle =
        if (isMore) 455f
        else 465f

      val useUntrainedMiddle =
        if (isMore) 317f
        else 330f
      val classSkillMiddle =
        if (isMore) 384f
        else 397.7f
      val classSkillIncrement = 8f

      val rageMiddle = 524f
      val favouredEnemyMiddle = 524f
      val acpWidth = 25f

      val numSlots = 
        if (isMore) 42
        else 44

      SkillLayout(firstLine, lineIncrement, lineBottomOffset, lineBoxHeight, skillsAreaLeft, skillsAreaRight, 
        skillNameLeft, skillNameIndent, abilityMiddle, abilityOffset, ranksMiddle,
        useUntrainedMiddle, classSkillMiddle, classSkillIncrement, acpWidth, numSlots,
        rageMiddle, favouredEnemyMiddle)
  }

  def writeIconic(canvas: PdfContentByte, writer: PdfWriter, slot: String, imgFilename: String, iconic: Option[IconicImage], character: CharacterData) {
    if (imgFilename != "") {
      println("Iconic image file: "+imgFilename)
      slot match {
        case "background" | "inventory" =>
          println("Adding iconic image to "+slot)
          canvas.setGState(defaultGstate)
          val imgLayer = new PdfLayer("Iconic image", writer)
          canvas.beginLayer(imgLayer)
          var copyrightX: Float = 0f
          var copyrightY: Float = 0f
          try {
            println("Image: "+imgFilename)
            val awtImage = java.awt.Toolkit.getDefaultToolkit().createImage(imgFilename)
            val img = Image.getInstance(awtImage, null)
            img.scaleToFit(180f,215f)
            slot match {
              case "inventory" => 
                img.setAbsolutePosition(315f - (img.getScaledWidth() / 2), 418f)
                copyrightX = 234f; copyrightY = 410f
              case "background" => 
                img.setAbsolutePosition(127f - (img.getScaledWidth() / 2), 425f)
                copyrightX = 30f; copyrightY = 420f
              case _ =>
            }
            canvas.addImage(img)

            for (i <- iconic; cp <- i.copyright) {
              println("Copyright notice on iconic: "+cp.copyright)
              if (copyrightX != 0) {
                val fadedGState = new PdfGState
                fadedGState.setBlendMode(PdfGState.BM_NORMAL)
                // fadedGState.setFillOpacity(0.8f)
                canvas.setGState(fadedGState)
                canvas.setFontAndSize(textFont, 7f)
                canvas.showTextAligned(Element.ALIGN_LEFT, "© "+cp.copyright, copyrightX, copyrightY, 0)
              }
            }
          } catch {
            case e: Exception => e.printStackTrace
          }
          canvas.endLayer()

        case "mini" =>
          println("Adding iconic image to "+slot)
          canvas.setGState(defaultGstate)
          val imgLayer = new PdfLayer("Iconic image", writer)
          canvas.beginLayer(imgLayer)
          try {
            println("Image: "+imgFilename)
            val awtImage = java.awt.Toolkit.getDefaultToolkit().createImage(imgFilename)
            val img = Image.getInstance(awtImage, null)

            // initiative marker
            img.scaleToFit(36f, 36f)
            img.setRotationDegrees(0)
            img.setAbsolutePosition(410.5f - (img.getScaledWidth() / 2), 716f - (img.getScaledHeight() / 2))
            canvas.addImage(img)

            // stat tracker
            img.scaleToFit(140f,150f)
            img.setRotationDegrees(180)
            img.setAbsolutePosition(122f - (img.getScaledWidth() / 2), 646f - (img.getScaledHeight() / 2))
            canvas.addImage(img)

            character.miniSize match {
              case "small" => 
                // stand-up figure
                img.scaleToFit(48f, 62f)
                img.setRotationDegrees(180)
                img.setAbsolutePosition(294f - (img.getScaledWidth() / 2), 726 - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                img.setRotationDegrees(0)
                img.setAbsolutePosition(294f - (img.getScaledWidth() / 2), 656f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                // square token
                img.scaleToFit(48f, 48f)
                img.setRotationDegrees(0)
                img.setAbsolutePosition(514.5f - (img.getScaledWidth() / 2), 127f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                img.setRotationDegrees(180)
                img.setAbsolutePosition(514.5f - (img.getScaledWidth() / 2), 181f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

              case "medium" =>
                // stand-up figure
                img.scaleToFit(66f, 89f)
                img.setRotationDegrees(180)
                img.setAbsolutePosition(295f - (img.getScaledWidth() / 2), 714 - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                img.setRotationDegrees(0)
                img.setAbsolutePosition(295f - (img.getScaledWidth() / 2), 620f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                // square token
                img.scaleToFit(66f, 66f)
                img.setRotationDegrees(0)
                img.setAbsolutePosition(514.5f - (img.getScaledWidth() / 2), 126f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                img.setRotationDegrees(180)
                img.setAbsolutePosition(514.5f - (img.getScaledWidth() / 2), 198f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

              case "large" =>
                // stand-up figure
                img.scaleToFit(135f, 180f)
                img.setRotationDegrees(180)
                img.setAbsolutePosition(294f - (img.getScaledWidth() / 2), 632 - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                img.setRotationDegrees(0)
                img.setAbsolutePosition(294f - (img.getScaledWidth() / 2), 445f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                // square token
                img.scaleToFit(135f, 135f)
                img.setRotationDegrees(0)
                img.setAbsolutePosition(475f - (img.getScaledWidth() / 2), 220f - (img.getScaledHeight() / 2))
                canvas.addImage(img)

                img.setRotationDegrees(180)
                img.setAbsolutePosition(475f - (img.getScaledWidth() / 2), 364f - (img.getScaledHeight() / 2))
                canvas.addImage(img)
              case _ =>
            }
          } catch {
            case e: Exception => e.printStackTrace
          }
          canvas.endLayer()

        case _ => 
      }
    }
  }

  def writeWatermark(canvas: PdfContentByte, writer: PdfWriter, watermark: String, colour: String, pageSize: Rectangle) {
    println("Adding watermark: "+watermark)

    val watermarkGstate = new PdfGState
    watermarkGstate.setBlendMode(PdfGState.BM_NORMAL)
    watermarkGstate.setFillOpacity(0.3f)
    canvas.setGState(watermarkGstate)

    canvas.beginText
    val watermarkLayer = new PdfLayer("Watermark", writer)
    canvas.beginLayer(watermarkLayer)
    // val font = BaseFont.createFont(BaseFont.HELVETICA, BaseFont.CP1252, BaseFont.EMBEDDED)
    canvas.setFontAndSize(altFont, (900f / watermark.length).toInt)
    //canvas.setColorFill(new BaseColor(0f, 0f, 0f))
    canvas.setColorFill(interpretColour(colour))

    val isPortrait = pageSize.getHeight() > pageSize.getWidth()
    val x = pageSize.getWidth() / 2 + (if (isPortrait) 60f else 30f)
    val y = pageSize.getHeight() / 2 - (if (isPortrait) 30f else 60f)
    val angle = if (isPortrait) 60f else 30f
    canvas.showTextAligned(Element.ALIGN_CENTER, watermark, x, y, angle)
    // canvas.showTextAligned(Element.ALIGN_CENTER, watermark, 365f, 400f, 60f)
    canvas.endLayer
    canvas.endText
  }

  def writeColourOverlay(canvas: PdfContentByte, colour: String, pageSize: Rectangle) {
    if (colour == "black") {
      val gstate = new PdfGState
      
      gstate.setBlendMode(PdfGState.BM_OVERLAY)
      //gstate.setFillOpacity(0.5f)
      canvas.setGState(gstate)
      canvas.setColorFill(new BaseColor(0.1f, 0.1f, 0.1f))
      canvas.rectangle(0f, 0f, pageSize.getWidth(), pageSize.getHeight())
      canvas.fill
      
      val gstate2 = new PdfGState
      gstate2.setBlendMode(PdfGState.BM_COLORDODGE)
      gstate2.setFillOpacity(0.5f)
      canvas.setGState(gstate2)
      canvas.setColorFill(new BaseColor(0.2f, 0.2f, 0.2f))
      canvas.rectangle(0f, 0f, pageSize.getWidth(), pageSize.getHeight())
      canvas.fill
      
      //  ...correct hilights...
    } else if (colour != "normal") {
      val gstate = new PdfGState
      gstate.setBlendMode(colour match {
          case "light" => PdfGState.BM_SCREEN
          case "dark" => PdfGState.BM_OVERLAY
          case "black" => PdfGState.BM_COLORBURN
          case _ => new PdfName("Color")
      })
      canvas.setGState(gstate)
      canvas.setColorFill(interpretColour(colour))
      canvas.rectangle(0f, 0f, pageSize.getWidth(), pageSize.getHeight())
      canvas.fill
    }
  }

  val encoding = BaseFont.IDENTITY_H
  def textFont = BaseFont.createFont("public/fonts/Roboto-Condensed.ttf", encoding, true)
  def textFontBold = BaseFont.createFont("public/fonts/Roboto-BoldCondensed.ttf", encoding, true)
  def altFont = BaseFont.createFont("public/fonts/Merriweather-Black.ttf", encoding, true)
  def barbarianFont = BaseFont.createFont("public/fonts/Amatic-Bold.ttf", encoding, true)
  def barbarianFont2 = BaseFont.createFont("public/fonts/dirty-duo.ttf", encoding, true)

  def interpretColour(colour: String): BaseColor = colour match {
    case "light" => new BaseColor(0.3f, 0.3f, 0.3f)
    case "dark" => new BaseColor(0.35f, 0.35f, 0.35f)
    case "black" => new BaseColor(0f, 0f, 0f)
    case "red" => new BaseColor(0.60f, 0.2f, 0.2f)
    case "orange" => new BaseColor(0.72f, 0.47f, 0.30f)
    case "yellow" => new BaseColor(1.0f, 0.92f, 0.55f)
    case "lime" => new BaseColor(0.77f, 0.85f, 0.55f)
    case "green" => new BaseColor(0.5f, 0.7f, 0.5f)
    case "cyan" => new BaseColor(0.6f, 0.75f, 0.75f)
    case "blue" => new BaseColor(0.55f, 0.63f, 0.80f)
    case "purple" => new BaseColor(0.80f, 0.6f, 0.70f)
    case "pink" => new BaseColor(1.0f, 0.60f, 0.65f)
    case _ => new BaseColor(0.3f, 0.3f, 0.3f)
  }


  def writeLogo(canvas: PdfContentByte, writer: PdfWriter, slot: String, gameData: GameData, character: Option[CharacterData], gmData: Option[GMData]) {
    lazy val imgFile = {
      character match {
        case Some(char) => logoImage(gameData, char)
        case None => 
          gmData match {
            case Some(gmdata) => logoImage(gameData, gmdata.logo)
            case None => gameData.defaultLogo
          }
      }
    }

    if (slot == "core" || slot == "eidolon") {
      canvas.setGState(defaultGstate)
      val imgLayer = new PdfLayer("Logo image", writer)
      canvas.beginLayer(imgLayer)
      try {
        println("Adding logo: "+imgFile)
        val awtImage = java.awt.Toolkit.getDefaultToolkit().createImage(imgFile)
        val img = Image.getInstance(awtImage, null)
        img.scaleToFit(170f,50f)
        img.setAbsolutePosition(127f - (img.getScaledWidth() / 2), 800f - (img.getScaledHeight() / 2))
        canvas.addImage(img)
      } catch {
        case e: Exception => e.printStackTrace
      }
      canvas.endLayer()
    }

    if (slot == "kingdom" || slot == "hex-a4" || slot == "hex-a3" || slot == "hex-a4-landscape" || slot == "iso-a4" || slot == "grid-a4") {
      canvas.setGState(defaultGstate)
      val imgLayer = new PdfLayer("Logo image", writer)
      canvas.beginLayer(imgLayer)
      try {
        println("Adding logo: "+imgFile)
        val awtImage = java.awt.Toolkit.getDefaultToolkit().createImage(imgFile)
        val img = Image.getInstance(awtImage, null)
        if (slot == "hex-a4-landscape") {
          img.scaleToFit(120f, 35f)
          img.setAbsolutePosition(80f - (img.getScaledWidth() / 2), 570f - (img.getScaledHeight() / 2))
        } else if (slot == "iso-a4" || slot == "grid-a4") {
          img.scaleToFit(120f, 35f)
          img.setAbsolutePosition(90f - (img.getScaledWidth() / 2), 810f - (img.getScaledHeight() / 2))
        } else {
          img.scaleToFit(170f,50f)
          img.setAbsolutePosition(127f - (img.getScaledWidth() / 2), 800f - (img.getScaledHeight() / 2))
        }
        canvas.addImage(img)
      } catch {
        case e: Exception => e.printStackTrace
      }
      canvas.endLayer()
    }
  }

  def logoImage(gameData: GameData, character: CharacterData): String = {
    character.customLogo.map{ logo =>
      val path = logo.getAbsolutePath
      println("Custom logo: "+path)
      path
    }.getOrElse{
      val fileName: String = character.logo.flatMap(_.fileName).getOrElse(
        gameData.game match {
          case "pathfinder" =>
            if (character.classes.exists(_.pages.exists(_.startsWith("core/neoexodus"))))
              "pathfinder/neoexodus.png"
            else
              "pathfinder/Pathfinder.png"
          case "dnd35" => "dnd35/dnd35.png"
          case _ => ""
        }
      )
      println("Non-custom logo: "+fileName)
      "public/images/logos/"+fileName
    }
  }

  def logoImage(gameData: GameData, logo: Option[Logo]): String = {
    val fileName: String = logo.flatMap(_.fileName).getOrElse(
      gameData.game match {
        case "pathfinder" => "pathfinder/Pathfinder.png"
        case "dnd35" => "dnd35/dnd35.png"
        case _ => ""
      }
    )
    "public/images/logos/"+fileName
  }


  def composeAll(character: CharacterData, gameData: GameData, folders: List[File], language: String): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val document = new Document
    val writer = PdfWriter.getInstance(document, out)
    writer.setRgbTransparencyBlending(true)
    document.open

    println("Building all pages for: "+gameData.name)

    val colour = character.colour
    for (page <- gameData.pages; pageFile <- locatePage(folders, page)) {
      println("Adding page: "+page.name)
      //val pageFile = new File(folder.getPath+"/"+page.file)
      val fis = new FileInputStream(pageFile)
      val reader = new PdfReader(fis)

      // get the right page size
      val template = writer.getImportedPage(reader, 1)
      val pageSize = reader.getPageSize(1)
      document.setPageSize(pageSize)
      document.newPage

      val canvas = writer.getDirectContent
      val baseLayer = new PdfLayer("Character Sheet", writer);
      canvas.beginLayer(baseLayer)
      canvas.setColorFill(BaseColor.WHITE)
      canvas.rectangle(0f, 0f, pageSize.getWidth(), pageSize.getHeight())
      canvas.fill

      //  the page
      canvas.addTemplate(template, 0, 0)

      //  copyright notice
      writeCopyright(canvas, writer, gameData, page)

      // skills
      if (page.slot == "core")
        writeSkills(canvas, writer, page, gameData, Some(character), language)
      if (page.slot == "eidolon")
        writeSkills(canvas, writer, page, gameData, Some(character.makeEidolon(gameData)), language)
      if (page.slot == "animalcompanion")
        writeSkills(canvas, writer, page, gameData, Some(character.makeAnimalCompanion(gameData)), language)


      //  generic image
      writeIconic(canvas, writer, page.slot, "public/images/iconics/generic.png", None, character)

      writeColourOverlay(canvas, colour, pageSize)

      canvas.endLayer()

      //  logo
      writeLogo(canvas, writer, page.slot, gameData, Some(character), None)

      //  watermark
      if (character.watermark != "") {
        writeWatermark(canvas, writer, character.watermark, colour, pageSize)
      }

      fis.close
    }

    document.close
    out.toByteArray
  }
}

class CharacterInterpretation(gameData: GameData, character: CharacterData) {
  case class PageSlot(slot: String, variant: Option[String]) {
    lazy val page: Option[Page] = {
      val ps = gameData.pages.toList.filter { p => p.slot == slot && p.variant == variant }
      ps.headOption
    }
    override def toString = variant match {
      case Some(v) => slot+"/"+v 
      case None => slot
    }
  }

  def pageSlot(name: String) = 
    name.split("/", 2).toList match {
      case page :: Nil => PageSlot(page, None)
      case page :: variant :: _ => PageSlot(page, Some(variant))
      case _ => throw new Exception("Wow. I guess that match really wasn't exhaustive.")
    }

  def selectCharacterPages(classes: List[GameClass]): List[Page] = {
    println(" -- Classes: "+classes.map(_.name).mkString(", "))
    val basePages = gameData.base.pages.toList.map(pageSlot)
    val classPages = classes.flatMap(_.pages).map(pageSlot)

    //  additional pages
    var pages = basePages ::: classPages
    if (character.includeCharacterBackground) {
      if (character.isPathfinderSociety)
        pages = pages ::: List(PageSlot("background", Some("pathfindersociety")))
      else
        pages = pages ::: List(PageSlot("background", None))
    }
    if (character.includeLycanthrope)
      pages = pages ::: List(PageSlot("lycanthrope", None))
    if (character.includeIntelligentItem)
      pages = pages ::: List(PageSlot("intelligent-item", None))
    if (character.includePartyFunds)
      pages = pages ::: List(PageSlot("partyfunds", None))
    if (character.includeAnimalCompanion) {
      pages = pages ::: List(PageSlot("animalcompanion", None))
      if (character.hasAnimalIconic)
        pages = pages ::: List(PageSlot("mini-animal", Some(character.miniAnimalSize)))
    }
    if (character.includeMini)
      pages = pages ::: List(PageSlot("mini", Some(character.miniSize)))

    println(" -- Base pages: "+basePages.map(_.toString).mkString(", "))
    println(" -- Class pages: "+classPages.map(_.toString).mkString(", "))
    var slotNames = pages.map(_.slot).distinct
    println(" -- Distinct slots: "+slotNames.mkString(", "))

    //  special cases
    if (character.hideInventory) {
      println("Removing inventory")
      pages = PageSlot("core", Some("simple")) :: PageSlot("combat", Some("simple")) :: pages
      slotNames = slotNames.filter(_ != "inventory")
    } else if (character.moreClasses) {
      pages = PageSlot("core", Some("more")) :: pages
    }

    if (slotNames.contains("spellbook")) {
      val spellbookPage = character.spellbookSize match {
        case "small" => PageSlot("spellbook", Some("small"))
        case "medium" => PageSlot("spellbook", None)
        case "large" => PageSlot("spellbook", Some("large"))
      }
      pages = pages.filter(_.page != "spellbook") ::: (spellbookPage :: Nil)
    }
    if (character.inventoryStyle != "auto") {
      val page = PageSlot("inventory", Some(character.inventoryStyle))
      if (page.page != None)
        pages = pages.filter(_.page != "inventory") ::: (page :: Nil)
    }

    println(" -- Final slots: "+slotNames.mkString(", "))
    pages = for (slotName <- slotNames) yield {
      val pageInstances = pages.filter( _.slot == slotName)
      val overridingInstances = pageInstances.filter(v => v.variant != None)
      val selectedInstance =
        if (overridingInstances == Nil)
          pageInstances.head
        else
          overridingInstances.head

      // println("Selecting page: "+slotName.toString)
      selectedInstance
    }
    
    println(" -- Selected "+pages.length+" pages: "+pages.map(_.toString).mkString(", "))
    val printPages = pages.toList.flatMap(_.page)
    println(" -- Printing "+printPages.length+" pages: "+printPages.map(_.name).mkString(", "))
    printPages
    //printPages.sortWith((a,b) => a.pagePosition < b.pagePosition)
  }

  def pages = {
    // var clsPages =
      if (character.partyDownload)
        character.classes.flatMap( cls => selectCharacterPages(List(cls)) )
      else
        selectCharacterPages(character.classes)

    // var pages = 
    //   if (character.includeGM) 
    //     clsPages ::: gameData.gm
    //   else
    //     clsPages

    // clsPages
  }
}
