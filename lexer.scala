package lexer
import scala.io.Source
import scala.util.matching.Regex
import java.util.regex.Pattern

class Token(var tokenType: String,  var key: String = "") {}

class Lexer {
  def lex(fileName: String): Iterator[Token] =   {
    val input = Source.fromFile(fileName).mkString
    val isInteger = "(-?[0-9]+)"
    val isID = "([a-z]+[a-zA-Z0-9_]*)"
    val isKeyword = "(def|if|then|else|skip|while|do|repeat|until|break|and|continue)"
    val isString = "([0-9a-zA-Z]+)"
    val isOp = "(;|\\(|\\)|==|=|<=|>=|<|>|,|\\{|\\}|:=|\\+|\\*|-|/)"
    val all = "(.*)"
    val isWhitespace = "(\\n|\\t|\\s|\\x00|^$)"
    val R = List(isWhitespace, isString,isInteger,isOp,all).mkString("|").r
    var tokens: List[Token] = List()
    for (pattern <- R.findAllMatchIn(input))  {
      pattern.group(0) match {
        case x if Pattern.matches(isKeyword,x) => tokens = tokens :+ new Token("keyword", x)
        case x if Pattern.matches(isID,x) => tokens = tokens :+ new Token("ID", x)
        case x if Pattern.matches(isInteger,x) => tokens = tokens :+ new Token("Integer", x)
        case x if Pattern.matches(isOp,x) => tokens = tokens :+ new Token("Operator", x)
        case x if Pattern.matches(isWhitespace,x) => 
        case x  => throw new Exception("Lexical error - Invalid Identifier: "+x.split(" ")(0))
      }
    }
    return tokens.toIterator
  }
}
