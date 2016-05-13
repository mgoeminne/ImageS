package mgoeminne.images.core

import java.io.File

/**
  * Created by mg on 6/05/16.
  */
object Main
{
   def main(args: Array[String])
   {
      val test = Image(new File("demo/images/interblocage.jpg"))

      val grey = test.asBinary()

      grey.draw("original")
      grey.verticalFlip.draw("modified")
   }
}
