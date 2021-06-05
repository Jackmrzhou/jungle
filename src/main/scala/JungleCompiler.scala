package c.y.z

import tokenizer.Tokenizer

import java.io.ByteArrayInputStream

object JungleCompiler {
  def main(args: Array[String]): Unit = {
    val program =
      s"""
         | +-  * /= 123
         |""".stripMargin

    val _tokenizer = new Tokenizer(new ByteArrayInputStream(program.getBytes()))

    while ( _tokenizer.hasNext ){
      val token = _tokenizer.Next()
      print(token)
    }
  }
}
