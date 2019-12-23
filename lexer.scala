import scala.io.Source
import scala.util.matching.Regex
import java.util.regex.Pattern

object Main {
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0)).mkString
    val isNumber = "(-?[0-9]+)"
    val isID = "([a-z]+[a-zA-Z0-9_]*)"
    val isKeyword = "([def|if|then|else|skip|while|do|repeat|until|break|and|continue])"
    val isString = "([0-9a-zA-Z]+)"
    val isOp = "(;|\\(|\\)|==|=|<=|>=|<|>|,|\\{|\\}|:=|\\+|\\*|-|/)"
    val R = List(isString,isNumber,isOp).mkString("|").r
    val tokens = List()
    for (pattern <- R.findAllMatchIn(input))  {
      pattern.group(0) match {
        case x if Pattern.matches(isKeyword,x) => println(x)
        case x if Pattern.matches(isID,x) => println(x)
        case x if Pattern.matches(isNumber,x) => println(x)
        case x if Pattern.matches(isOp,x) => println(x)
        case x => println(x)
      }
    }
  }
}
