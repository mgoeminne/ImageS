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

      val channels = test.decompose

      channels._1.draw("alpha")
      channels._2.draw("red")
      channels._3.draw("green")
      channels._4.draw("blue")



   }

}
