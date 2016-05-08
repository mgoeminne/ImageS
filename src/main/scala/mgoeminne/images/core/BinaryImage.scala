package mgoeminne.images.core

import java.awt.image.BufferedImage

/**
  * Binary coded image.
  */
class BinaryImage(buffer: BufferedImage) extends Image(buffer)
{
   override def asBinary() = this
}

object BinaryImage
{
   def apply(pixels: Array[Boolean], width: Int, height: Int): BinaryImage =
   {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
      ret.getRaster.setPixels(
         0,
         0,
         width,
         height,
         pixels.map(p => if(p) 0 else 1))

      new BinaryImage(ret)
   }
}
