package c.y.z

import tokenizer.{JgToken, Tokenizer}

import c.y.z.parser.JgParser

import java.io.ByteArrayInputStream
import scala.collection.mutable.ListBuffer

object JungleCompiler {
  def main(args: Array[String]): Unit = {
    val program =
      s"""
         | 1 + 2 * 4 + 2 / 2
         |""".stripMargin

    val _tokenizer = new Tokenizer(new ByteArrayInputStream(program.getBytes()))

    val tokens = ListBuffer[JgToken]()
    while ( _tokenizer.hasNext ){
      val token = _tokenizer.Next()
      tokens += token
      print(token)
    }
    println(JgParser(tokens.toList))
  }
}
