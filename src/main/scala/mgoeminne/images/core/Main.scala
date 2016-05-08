package mgoeminne.images.core

import java.io.File

/**
  * Created by mg on 6/05/16.
  */
object Main
{
   def main(args: Array[String])
   {
      val test = Image(new File("/Users/mg/Documents/interblocage.jpg"))

      val grey = test.asGreyScale()

      val t = test.decompose

      val a = t._1
      val r = t._2
      val g = t._3
      val b = t._4

      val recompose = RGBImage(a, r, g, b)

      test.draw("original")
      recompose.draw("recomposed")
   }
}
