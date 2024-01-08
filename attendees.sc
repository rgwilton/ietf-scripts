#!/usr/bin/env -S scala-cli shebang -S 3

//> using scala "3.3.1"

//> using lib "com.lihaoyi::mainargs:0.5.4"
//> using lib "com.lihaoyi::fastparse:3.0.0"
//> using lib "com.lihaoyi::os-lib:0.9.1"
//> using lib "net.ruippeixotog::scala-scraper:3.1.0"

import os.Path
import mainargs.*
import fastparse.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

@main(doc = 
  "Extracts review comments, inc section and paragraph.\n\n\n" + 
  """|One line comments start with #, ##, or ###.
     |Multiline comments are 'START #/##/###' and 'END #/##/###.'""".stripMargin)
def attendees(@arg(doc = "Annotated text file")
                            draft: Option[String] = None,
                            @arg(doc = "Extraction stats (for debugging)")
                            stats: Flag,
                            @arg(doc = "Generate the output in github issue style")
                            gitStyle: Flag,                         
                            @arg(doc = "Dump raw extracted blocks (for debugging)")
                            `dump-blocks`: Flag) = {
  System.out.println("Running")

  val browser = JsoupBrowser()
  val doc = browser.get("https://registration.ietf.org/118/participants/")

  val rows = doc >> elementList("div div table tbody tr")
  case class Attendee(firstName: String, lastName: String, org: String, country: String, inPerson: Boolean)
  val attendees = 
    for row <- rows yield
      val cols = row >> elementList("tr td")
      val lastName = cols(0).ownText
      val firstName = cols(1).ownText
      val org = cols(2).ownText
      val country = cols(3).ownText
      val regType = cols(4).ownText
      Attendee(firstName, lastName, org, country, regType == "Onsite")

  // Map from surname to attendee.
  val inPersonAttendeesMap = attendees.filter(_.inPerson).groupBy(_.lastName.toLowerCase)
  val remoteAttendeesMap = attendees.filterNot(_.inPerson).groupBy(_.lastName.toLowerCase)

  case class PossibleAttendee(firstName: String, lastName: String, email: String, alternativeFirstNames: Seq[String]):
    override def toString(): String = s"$firstName $lastName, $email"

  val possibleAttendees = 
    val lines = scala.io.Source.fromFile("possible_attendees.txt").getLines
    (for line <- lines yield
      line.split(",") match {
        case Array(first, last, email, tail @ _*) => 
          Some(PossibleAttendee(first.strip, last.strip, email.strip, tail.map(_.strip)))
        case _ =>
          System.err.println(s"Could not extract '$line'")
          None
      })
    .flatten
    .toSeq

  val inPersonYangFolk = possibleAttendees.filter {
    pa => 
      inPersonAttendeesMap.get(pa.lastName.toLowerCase) match
        case Some(attendees) if attendees.exists{a => pa.firstName.toLowerCase == a.firstName.toLowerCase} => true
        case _ => false
  }
  val inPersonMaybeYangFolk = possibleAttendees.filter {
    pa => 
      inPersonAttendeesMap.get(pa.lastName.toLowerCase) match
        case Some(attendees) if attendees.forall(a => pa.firstName.toLowerCase != a.firstName.toLowerCase) => true
        case _ => false
  }
  val remoteYangFolk = possibleAttendees.filter {
    pa => remoteAttendeesMap.contains(pa.lastName.toLowerCase)
  }
  val unregisteredYangFolk = possibleAttendees.filterNot {
    pa =>
      inPersonAttendeesMap.contains(pa.lastName.toLowerCase) ||
      remoteAttendeesMap.contains(pa.lastName.toLowerCase)
  }
  
  println(inPersonYangFolk.mkString("Dinner Candidates:\n", ",\n", "\n"))
  println(inPersonMaybeYangFolk.mkString("Potential Dinner Candidates:\n", ",\n", "\n"))
  println(remoteYangFolk.mkString("Registered Remote:\n", ",\n", "\n"))
  println(unregisteredYangFolk.mkString("Not Registered:\n", ",\n", "\n"))
}

// Hack to make main-args work with scala-cli.
ParserForMethods(this).runOrExit(args.toIndexedSeq)