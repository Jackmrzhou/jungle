package c.y.z

import tokenizer.{EOF, JgToken, Tokenizer}

import c.y.z.parser.JgParser
import c.y.z.type_checker.{Checker, Scope}

import java.io.{BufferedReader, ByteArrayInputStream, InputStream, InputStreamReader}
import scala.collection.mutable.ListBuffer

object JungleCompiler {
  def main(args: Array[String]): Unit = {
    val program =
      s"""
         | var test int32
         | var a,b,c int32
         | var d,e = 1,2
         | var f,g int32 = 1,2
         | x := 1
         | a,b := 1,2
         | var s []int32
         | var s int32
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
    JgParser(tokens.toList) match {
      case Left(value) => println(s"ERROR: $value")
      case Right(ast) => Checker.check(ast, new Scope(ListBuffer[Scope](), null)) match {
        case Some(value) => println(s"Type Checking failed, err=$value"); println(ast)
        case None => println(ast)
      }
    }
  }
}
