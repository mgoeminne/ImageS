package mgoeminne.images.core

import java.awt.image.{BufferedImage, DataBufferByte}

/**
  * Binary coded image.
  */
class BinaryImage(buffer: BufferedImage) extends Image[BinaryImage, Boolean](buffer)
{
   override def asBinary() = this

   override def makeImage(pixels: Array[Int], width: Int, height: Int) =
   {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
      ret.getRaster.setPixels(0, 0, width, height, pixels)

      new BinaryImage(ret)
   }

   def asInt(value: Boolean) = value match {
      case true => 1
      case false => 0
   }

   override def equals(that: Any) = {
      that match {
         case x: BinaryImage => this.intPixels.deep == x.intPixels.deep
         case _ => false
      }
   }
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
