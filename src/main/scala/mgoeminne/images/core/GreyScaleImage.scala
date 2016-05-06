package mgoeminne.images.core

import java.awt.image.{BufferedImage, DataBufferByte, Raster}

/**
  * A GreyScale image
  */
class GreyScaleImage(buffer: BufferedImage) extends Image(buffer)
{
   override def asGreyScale(): GreyScaleImage = this

   /**
     * Linearly the image in such a way the lowest grey tone becomes 0
     * and the highest grey tone becomes 1.
     */
   def normalized() =
   {
      val array = pixels
      val min = array.min
      val max = array.max

      println(pixels.mkString(" "))
      println(min + " " + max)
      println(array.map(p => f(p, min, max, -128, 127)).mkString(" "))


      def f(x:Byte, min: Byte, max: Byte, threshold_min: Byte, threshold_max: Byte) =
      {
         val rangeOrig = (max-min).toFloat
         val rangeDest = (threshold_max - threshold_min).toFloat
         val ratio = (x-min) / rangeOrig

         threshold_min + ratio*rangeDest
      }

      val ret = new BufferedImage(buffer.getWidth(), buffer.getHeight(), BufferedImage.TYPE_BYTE_GRAY)

      ret.getRaster.setPixels(
         0,
         0,
         buffer.getWidth,
         buffer.getHeight,
         array.map(p => f(p, min, max, -128, 127)))


      new GreyScaleImage(ret)
   }

   private def pixels = this.buffer.getRaster().getDataBuffer() match {
      case x : DataBufferByte => x.getData
   }
}
