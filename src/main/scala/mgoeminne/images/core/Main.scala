package mgoeminne.images.core

import java.awt.Color
import java.io.File

import mgoeminne.images.core.mask.Mask

/**
  * Created by mg on 6/05/16.
  */
object Main
{
   def main(args: Array[String])
   {
      val test = RGBImage(new File("demo/images/interblocage.jpg"))
      val mask = Mask.polygon(test.width, test.height, Seq((0,0), (100, 100), (50, 100)))
      //val masked = test(mask)

      test.maskToValue(mask, (Byte.MaxValue,Byte.MinValue,Byte.MinValue)).draw()


   }
}
