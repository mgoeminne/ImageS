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

      val grey = test.asGreyScale

      //grey.draw("original")
      //grey.rotate90.draw("90")
      //grey.rotate180.draw("180")
      grey.rotate270.draw("270")
   }
}
