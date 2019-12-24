import scala.io.Source
import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.sys

class Token(var tokenType: String,  var key: String = "") {}

object Main {
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0)).mkString
    val isInteger = "(-?[0-9]+)"
    val isID = "([a-z]+[a-zA-Z0-9_]*)"
    val isKeyword = "(def|if|then|else|skip|while|do|repeat|until|break|and|continue)"
    val isString = "([0-9a-zA-Z]+)"
    val isOp = "(;|\\(|\\)|==|=|<=|>=|<|>|,|\\{|\\}|:=|\\+|\\*|-|/)"
    val all = "(.*)"
    val R = List(isString,isInteger,isOp,all).mkString("|").r
    val tokens = List()
    for (pattern <- R.findAllMatchIn(input))  {
      pattern.group(0) match {
        case x if Pattern.matches(isKeyword,x) => tokens :+ new Token("keyword", x)
        case x if Pattern.matches(isID,x) => tokens :+ new Token("ID", x)
        case x if Pattern.matches(isInteger,x) => tokens :+ new Token("Integer", x)
        case x if Pattern.matches(isOp,x) => tokens :+ new Token("Operator", x)
        case x if x != sys.props("line.separator") => throw new Exception("Lexical error - Invalid Identifier: "+x)
        case x => 
      }
    }
  }
}
