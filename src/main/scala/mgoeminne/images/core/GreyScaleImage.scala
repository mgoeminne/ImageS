package mgoeminne.images.core

import java.awt.Color
import java.awt.image.{BufferedImage, DataBufferByte, Raster}

/**
  * A grey scale image, in which each pixel is expressed as a nuance of grey
  * (safe for work).
  */
class GreyScaleImage(buffer: Array[Byte], width: Int) extends SingleLayerImage[GreyScaleImage, Byte](
   buffer,
   width,
   buffer.size / width)
{
   /**
     * Transforms the image in such a way the lowest grey tone takes the lowest value,
     * and the highest grey tone takes the highest value.
     */
   def normalized() =
   {
      val min = buffer.min
      val max = buffer.max

      def f(x:Byte, min: Byte, max: Byte) =
      {
         val rangeOrig = max - min
         val ratio = (x-min) / rangeOrig

         (Byte.MinValue + (x*ratio)).toByte
      }

      new GreyScaleImage(buffer.map(pixel => f(pixel, min, max)), width)
   }

   /**
     * Reverses the colors of this image: white becomes black, and black becomes white.
     * @return The reversed version of this image.
     */
   def reverse: GreyScaleImage = new GreyScaleImage(buffer.map(pixel => (Byte.MaxValue - (pixel - Byte.MinValue)).toByte), width)

   /**
     * Transforms this image into a RGB image, the grey scale being replaced by
     * a color gradient.
     * @return A color image corresponding to a color mapping of this image.
     */
   def toRGB(): RGBImage = new RGBImage(this, this, this)

   def toBufferedImage() =
   {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
      ret.getRaster.setPixels(
         0,
         0,
         width,
         height,
         buffer.map(x => x.toInt - Byte.MinValue))

      ret
   }

   override protected def makeImage(buffer: Array[Byte], width: Int): GreyScaleImage = new GreyScaleImage(buffer, width)

   override def bgColor: Byte = Byte.MinValue
}

object GreyScaleImage{
   def apply(width: Int, height: Int, color: Byte) = new GreyScaleImage(Array.fill(width*height)(color), width)
}