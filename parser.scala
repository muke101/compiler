import lexer
import scala.Option

class Node(var key: Token = lexer.Token(), var children: List[Node] = List())  {}

object Main {
  def main(args: Array[String]) {
    val Lexer = new lexer.Lexer 
    val tokens = Lexer.lex(args(0))
  } 

  def raise(value: String)  {
    throw new Exception("Syntax error: "+value)
  }

  def parseBlock(tl: Iterator[Token], node: Node = Node()): Option[(Iterator[Token], Node)] = {
    node.key = 'BLOCK'
    var token = tl.next()

    if (token.key == "{") {
      node.children += Node(token)
      parseEme(tl) match {
        case Some((t,n)) => {
              node.children += n
              token = t.next()
              if (token.key == "}") {
                node.children += Node(token)
              }
              else  {
                println("Syntax error: ",token.key)
                return None
              }
            }
        case None => {
              println("Syntax error: ",token.key)
              return None
            }
        }
    }
    else  {
      println("Syntax error: ",token.key)
      return None
    }

    return Some((tl,node))
  } 

  def ParseEne(tl: Iterator[Token], node: Node = Node()): Option[(Iterator[Token], Node)] =  {
    node.key = "EnE"  
    var token = tl.next()
    
    parseE(tl) match  {
      case Some((t,n)) => {
            node.children += n
            if (!t.isEmpty()) {
              token = t.next()
              if (token.key == ";") { 
                parseEne(t) match  {
                  case Some((t,n)) => {
                        node.children += n
                        return Some((t,node)) 
                      }
                  case None => {
                        println("Syntax error: "+token.key)
                        return None
                      }
                  }
              }
            }
            else  {
              return Some((t,node))
            }
        }
    } 
  }

  def ParseE(tl: Iterator[Token], node: Node = Node()): Option[(Iterator[Token], Node)] = {
    node.key = "E"
    var token = tl.next()

    if (token.tokenType == "Integer" || token.key == "skip") {
      node.children += Node(token)
      return Some(tl,node)
    }
    else {
      parseBlock(tl) match  {
        case Some((t,n)) => {
              node.children += n
              return Some((t,node))
            }
        case None => {
              println("Syntax error: "+token.key)
              return None
            }
        }
    }
  } 
}
