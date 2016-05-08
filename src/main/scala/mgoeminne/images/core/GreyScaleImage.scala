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

      def f(x:Float, min: Float, max: Float) =
      {
         val rangeOrig = max - min
         val ratio = (x-min) / rangeOrig

         ratio
      }

      val ret = new BufferedImage(buffer.getWidth(), buffer.getHeight(), BufferedImage.TYPE_BYTE_GRAY)

      ret.getRaster.setPixels(
         0,
         0,
         buffer.getWidth,
         buffer.getHeight,
         array.map(p => f(p, min, max)))


      new GreyScaleImage(ret)
   }

   /**
     * Transforms this image by applying a [0,1] -> [0,1] function to each of its pixels.
     * @param f A function defining the transformation of each of the pixels. Must be [0,1] -> [0,1]
     * @return A new greyscale function, corresponding to this after the transformation function
     *         has been applied to each of its pixels.
     */
   def transform(f: Float => Float): GreyScaleImage =
   {
      GreyScaleImage(pixels.map(f), width, height)
   }

   def reverse: GreyScaleImage = transform(x => 1-x)

   def pixels = this.buffer.getRaster().getDataBuffer match {
      case x: DataBufferByte => x.getData map GreyScaleImage.byte2Float
   }
}

object GreyScaleImage
{
   private def float2Byte(pixel: Float): Byte =
   {
      val range = (Byte.MaxValue.toFloat - Byte.MinValue.toFloat)
      val value = (range * pixel) + Byte.MinValue

      value.toByte
   }

   /**
     * Transforms a pixel value in byte into a pixel value in [0,1]
     * @param pixel a pixel value expressed as a byte.
     * @return the same value expressed as a float in [0,1]
     */
   private def byte2Float(pixel: Byte) = (pixel - Byte.MinValue).toFloat / (Byte.MaxValue.toFloat - Byte.MinValue.toFloat)

   /**
     * Generates a new greyscale image based on an array of float, each of them being in [0,1]
     * @param pixels the array of pixels expressed as values in [0,1]
     * @param width the width of image
     * @param height the height of image
     * @return a new greyscale image
     */
   def apply(pixels: Array[Float], width: Int, height: Int) =
   {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
      ret.getRaster.setPixels(
         0,
         0,
         width,
         height,
         pixels.map(p => float2Byte(p).toInt))

      new GreyScaleImage(ret)
   }
}
