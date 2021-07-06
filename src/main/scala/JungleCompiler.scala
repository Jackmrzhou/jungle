package c.y.z

import tokenizer.{EOF, JgToken, Tokenizer}

import c.y.z.parser.JgParser

import java.io.{BufferedReader, ByteArrayInputStream, InputStream, InputStreamReader}
import scala.collection.mutable.ListBuffer

object JungleCompiler {
  def main(args: Array[String]): Unit = {
    val program =
      s"""
         | 15 + 2.1 * ( 4 + 2 * 3 ) / 1.5 + 15.6
         | 15
         | a b c
         | a,b=1,2
         | var test int
         | var a,b,c int
         | var a,b = 1,2
         | var c,d int = 1,2
         | x := 1
         | a,b := 1,2
         |""".stripMargin

    val _tokenizer = new Tokenizer(new BufferedReader(new InputStreamReader(new ByteArrayInputStream(program.getBytes()))))

    val tokens = ListBuffer[JgToken]()
    while ( _tokenizer.HasNext() ){
      val token = _tokenizer.Next()
      if (token != EOF) {
        tokens += token
      }
      println(token)
    }
    println(JgParser(tokens.toList))
  }
}
