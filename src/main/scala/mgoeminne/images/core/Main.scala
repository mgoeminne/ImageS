package mgoeminne.images.core

import java.awt.Color
import java.io.File

/**
  * Created by mg on 6/05/16.
  */
object Main
{
   def main(args: Array[String])
   {
      val test = RGBImage(new File("demo/images/interblocage.jpg"))

      test.draw()
      test.reverse.draw()
   }
}
