#!/usr/bin/env -S scala-cli shebang -S 3

//> using scala "3.3.1"

//> using lib "com.lihaoyi::mainargs:0.5.1"
//> using lib "com.lihaoyi::fastparse:3.0.0"
//> using lib "com.lihaoyi::os-lib:0.9.1"

import os.Path
import mainargs.*
import fastparse.*


// Split into paragraph, comments, and section markers.
trait Block
case class ReviewComment(comment: String) extends Block
case class DocSectionHeader(section: String) extends Block
case class PageBreak(pageNumber: Int, extraText: String) extends Block
case class DocPara(text: String) extends Block
type Blocks = Seq[Block]

object Parser {
  import NoWhitespace._

  def digits[_p: P] = P(CharsWhileIn("0-9"))
  def whitespace[_p: P] = P(CharsWhileIn(" \t"))
  def blockBreak[_p: P] = P((whitespace.? ~ ("\n" | "\r\n")).rep(2))
  def blockReviewComment[_p: P] = 
    P(("START" ~ " ".? ~ ("#".rep(1) ~/ 
      (!("END" ~ " ".? ~ "#".rep(1)) ~ AnyChar).rep).! ~ 
      "END" ~/ " ".? ~ "#".rep(1) ~/ (!blockBreak ~ AnyChar).rep)).map(ReviewComment.apply)
  def normalReviewComment[_p: P] = P(("#".rep(1) ~/ (!blockBreak ~ AnyChar).rep).!).map(ReviewComment.apply)
  def appendixHdr[_p: P] = P("Appendix " ~/ (!blockBreak ~ AnyChar).rep).!.map(DocSectionHeader.apply)

  def docSectionHdr[_p: P] = P((digits ~ ".").rep(1) ~ "  " ~/ (!blockBreak ~ AnyChar).rep).!.map(DocSectionHeader.apply)
  def pageBreak[_p: P] = P((!("\n" | "Expires") ~ AnyChar).rep ~ "Expires" ~ (!("\n" | "[P") ~ AnyChar).rep ~ "[Page " ~ digits.!.map(_.toInt) ~/ "]\n" ~ "\f\n".? ~ (!"\n" ~ AnyChar).rep.!).map{ case (int, str) => PageBreak(int, str) }
  def docPara[_p: P] = P((!blockBreak ~ AnyChar).rep).!.map(DocPara.apply)

  def file[_p: P]: P[Seq[Block]] = 
    P(Start ~ (blockReviewComment | normalReviewComment | docSectionHdr | appendixHdr | pageBreak | docPara).rep(sep = blockBreak) ~ End)
}

// Output comment class.
case class Comment(sev: String, comment: String, page: Int, section: String, quotedPara: String)

def parseFile(draft: Path): Blocks =
  parse(os.read(draft), Parser.file(_)) match {
    case p:Parsed.Failure => System.err.println(s"Failed to parse $draft:\n${p.trace().longMsg}"); Seq()
    case p:Parsed.Success[Blocks] => p.value
  }

def extractComments(blocks: Seq[Block]) = {
  var curPage = 0
  var lastPara = ""
  var firstParaOnPage = false
  var curSection = ""
  blocks.flatMap{
    case PageBreak(pageNo, _) => 
      curPage = pageNo
      firstParaOnPage = true
      None
    case DocSectionHeader(header) =>
      curSection = header
      firstParaOnPage = false
      lastPara = ""
      None
    case DocPara(text) => 
      if (firstParaOnPage)
        lastPara = lastPara + "\n" + text
      else
        lastPara = text
      firstParaOnPage = false
      None      
    case ReviewComment(comment) =>
      firstParaOnPage = false
      val sev = comment.takeWhile(_ == '#').size match {
        case 1 => "Moderate"
        case 2 => "Minor"
        case 3 => "Nit"
      }
      val commentText = comment.dropWhile(_ == '#').strip()
      Some(Comment(sev, commentText, curPage, curSection, lastPara))
  }
}

def formatCommentsAdReview(allComments: Seq[Comment]) = {
  var commentNo = 0
  def formatComment(c: Comment) = {
    commentNo += 1
    s"($commentNo) p ${c.page}, sec ${c.section}\n\n" +
    (if (c.quotedPara.nonEmpty) c.quotedPara + "\n\n" else "") +
    c.comment + "\n" 
  }

  // Split and organize into categories by severity.
  Seq("Moderate", "Minor", "Nit")
  .map { category =>
    val comments = allComments.filter(_.sev == category)
    comments.map(formatComment).mkString(s"$category level comments:\n\n", "\n\n", "\n")
  }.mkString("\n\n")
}

def formatCommentsIesgReview(allComments: Seq[Comment]) = {
  var commentNo = 0
  def formatComment(c: Comment) = {
    commentNo += 1
    s"($commentNo) p ${c.page}, sec ${c.section}\n\n" +
    (if (c.quotedPara.nonEmpty) c.quotedPara + "\n\n" else "") +
    c.comment + "\n" 
  }

  // Split and organize into categories by severity.
  Seq("Moderate", "Minor", "Nit")
  .map { category =>
    val comments = allComments.filter(_.sev == category)
    comments.map(formatComment).mkString(s"$category level comments:\n\n", "\n\n", "\n")
  }.mkString("\n\n")
}

def printStats(blocks: Blocks) = {
   println(s"Extraction stats:")
   println(s"  Review comments = ${blocks.count(_.isInstanceOf[ReviewComment])}")
   println(s"  Page break = ${blocks.count(_.isInstanceOf[PageBreak])}")
   println(s"  Doc paragraphs = ${blocks.count(_.isInstanceOf[DocPara])}")
   println(s"  Section headers = ${blocks.count(_.isInstanceOf[DocSectionHeader])}")
   println("\n\n")
}

@main(doc = 
  "Extracts review comments, inc section and paragraph.\n\n\n" + 
  """|One line comments start with #, ##, or ###.
     |Multiline comments are 'START #/##/###' and 'END #/##/###.'""".stripMargin)
def extract_review_comments(@arg(doc = "Annotated text file")
                            draft: String,
                            @arg(doc = "Extraction stats (for debugging)")
                            stats: Flag,
                            @arg(doc = "Generate the output in github issue style")
                            gitStyle: Flag,                         
                            @arg(doc = "Dump raw extracted blocks (for debugging)")
                            `dump-blocks`: Flag) = {
  System.out.println("Running")

  val draftPath = Path(draft)
  if (!draftPath.toIO.exists) {
      System.err.println(s"File '$draft' not found")
      System.exit(-1)
  }

  val blocks = parseFile(draftPath)


  if (`dump-blocks`.value) println(blocks.mkString("Blocks:\n", "\n", "\n\n"))
  if (stats.value) printStats(blocks)

  val comments = extractComments(blocks)
  val formattedComments = 
    if (gitStyle.value)
      formatCommentsIesgReview(comments)
    else
      formatCommentsAdReview(comments)
  println(formattedComments)
}

// Hack to make main-args work with scala-cli.
ParserForMethods(this).runOrExit(args.toIndexedSeq)