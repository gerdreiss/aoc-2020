import cats.implicits.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

val fields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

val byrR: Regex   = """byr:(\d{4})""".r
val iyrR: Regex   = """iyr:(\d{4})""".r
val eyrR: Regex   = """eyr:(\d{4})""".r
val hgtCmR: Regex = """hgt:(\d{3})cm""".r
val hgtInR: Regex = """hgt:(\d{2})in""".r
val hclR: Regex   = """hcl:(#[0-9a-f]{6})""".r
val eclR: Regex   = """ecl:(amb|blu|brn|gry|grn|hzl|oth)""".r
val pidR: Regex   = """pid:([0-9]{9})""".r
val cidR: Regex   = """cid:([0-9]{3})""".r

enum EyeColor:
  case AMB, BLU, BRN, GRY, GRN, HZL, OTH

enum HeightUnit:
  case CM, IN

case class Height(h: Int, unit: HeightUnit)
case class HairColor(code: String)
case class Pass(
    byr: Int,
    iyr: Int,
    eyr: Int,
    hgt: Height,
    hcl: HairColor,
    ecl: EyeColor,
    pid: String,
    cid: Option[String] = None
)

enum PassToken:
  case BYR(year: Int)
  case EYR(year: Int)
  case IYR(year: Int)
  case HGT(height: Height)
  case HCL(color: HairColor)
  case ECL(color: EyeColor)
  case PID(pid: String)
  case CID(cid: String)

object PassLexer extends RegexParsers:

  val byrP: Parser[PassToken]   = byrR ^^ { v => PassToken.BYR(v.drop(4).toInt) }
  val iyrP: Parser[PassToken]   = iyrR ^^ { v => PassToken.IYR(v.drop(4).toInt) }
  val eyrP: Parser[PassToken]   = eyrR ^^ { v => PassToken.EYR(v.drop(4).toInt) }
  val hgtCmP: Parser[PassToken] = hgtCmR ^^ { v => PassToken.HGT(Height(v.slice(4, 7).toInt, HeightUnit.CM)) }
  val hgtInP: Parser[PassToken] = hgtInR ^^ { v => PassToken.HGT(Height(v.slice(4, 6).toInt, HeightUnit.IN)) }
  val hgtP: Parser[PassToken]   = hgtCmP | hgtInP
  val hclP: Parser[PassToken]   = hclR ^^ { v => PassToken.HCL(HairColor(v.drop(4))) }
  val eclP: Parser[PassToken]   = eclR ^^ { v => PassToken.ECL(EyeColor.valueOf(v.drop(4).toUpperCase)) }
  val pidP: Parser[PassToken]   = pidR ^^ { v => PassToken.PID(v.drop(4)) }
  val cidP: Parser[PassToken]   = cidR ^^ { v => PassToken.CID(v.drop(4)) }

  def token: Parser[PassToken] = byrP | iyrP | eyrP | hgtP | hclP | eclP | pidP | cidP

  import PassToken.*
  def pass: Parser[Option[Pass]] = phrase(repNM(7, 8, token)) ^^ { tokens =>
    var r = tokens
      .map {
        case v @ BYR(year) if (1920 to 2002).contains(year) => v.some
        case v @ EYR(year) if (2010 to 2020).contains(year) => v.some
        case v @ IYR(year) if (2020 to 2030).contains(year) => v.some
        case v @ HGT(height)                                =>
          height.unit match
            case HeightUnit.CM if (150 to 193).contains(height.h) => v.some
            case HeightUnit.IN if (59 to 76).contains(height.h)   => v.some
            case _                                                => None
        case v @ HCL(color)                                 => v.some
        case v @ ECL(color)                                 => v.some
        case v @ PID(pid)                                   => v.some
        case v @ CID(cid)                                   => v.some
        case _                                              => None
      }
      .sequence
      .map(_.sortBy(_.ordinal))
      .flatMap {
        case PassToken.BYR(byr) ::
            PassToken.IYR(eyr) ::
            PassToken.EYR(iyr) ::
            PassToken.HGT(hgt) ::
            PassToken.HCL(hcl) ::
            PassToken.ECL(ecl) ::
            PassToken.PID(pid) ::
            tail =>
          Some(
            Pass(
              byr,
              iyr,
              eyr,
              hgt,
              hcl,
              ecl,
              pid,
              tail.headOption.flatMap {
                case PassToken.CID(cid) => Some(cid)
                case _                  => None
              }
            )
          )
        case _ => None
      }
    None
  }
  def emptyLine: Parser[String]  = """\n\n""".r

  def apply(s: String): Either[String, Option[Pass]] =
    parse(pass, s) match
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _)  => Left(msg)

def readInput: Array[String] =
  io.Source
    .fromResource("example.txt")
    .mkString
    .split("\n\n")

def validateViaRegex: Int =
  readInput
    .count { pass =>
      // part one:
      // fields
      //   .map(_ + ":")
      //   .forall(pass.contains)
      // part two:
      pass
        .split("\\s+")
        .foldLeft(0) { (acc, s) =>
          s match
            case byrR(year) if (1920 to 2002).contains(year.toInt)     => acc + 1
            case iyrR(year) if (2010 to 2020).contains(year.toInt)     => acc + 1
            case eyrR(year) if (2020 to 2030).contains(year.toInt)     => acc + 1
            case hgtCmR(height) if (150 to 193).contains(height.toInt) => acc + 1
            case hgtInR(height) if (59 to 76).contains(height.toInt)   => acc + 1
            case hclR(_)                                               => acc + 1
            case eclR(_)                                               => acc + 1
            case pidR(_)                                               => acc + 1
            case _                                                     => acc
        } == fields.length
    }

def validateViaParser: Int =
  readInput
    .count { pass =>
      PassLexer(pass).isRight
    }

@main def main(): Unit =
  println(s"Validated via regex : $validateViaRegex")
  println(s"Validated via parser: $validateViaParser")
