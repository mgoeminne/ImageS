package mgoeminne.images.core

import java.awt.image.{BufferedImage, DataBufferByte, DataBufferInt}

import scala.concurrent.{Await, Future}

/**
  * A ARGB based image.
  */
case class RGBImage(buffer: BufferedImage) extends Image(buffer)
{
   override def asRGB() = this

   private def argb2alpha(pixel: Int) = pixel>>24 & 0xff
   private def argb2red(pixel: Int) = pixel >>16 & 0xff
   private def argb2green(pixel: Int) = pixel>>8 & 0xff
   private def argb2blue(pixel: Int) = pixel & 0xff

   /**
     * Extract the red component of the image.
 *
     * @return the greyscale image corresponding to the red component of this image.
     */
   def red(): GreyScaleImage = pixels2Channel(intPixels, argb2red)

   /**
     * Extract the green component of the image.
 *
     * @return the greyscale image corresponding to the green component of this image.
     */
   def green(): GreyScaleImage = pixels2Channel(intPixels, argb2green)

   /**
     * Extract the blue component of the image.
 *
     * @return the greyscale image corresponding to the blue component of this image.
     */
   def blue(): GreyScaleImage = pixels2Channel(intPixels, argb2blue)

   /**
     * Extract the alpha component of the image.
 *
     * @return the greyscale image corresponding to the alpha component of this image.
     */
   def alpha(): GreyScaleImage = pixels2Channel(intPixels, argb2alpha)

   /**
     * Decomposes this ARGB image into its four channels.
 *
     * @return The greyscale images corresponding to the alpha, red, green, and blue components,
     *         respectively.
     */
   def decompose: (GreyScaleImage, GreyScaleImage, GreyScaleImage, GreyScaleImage) =
   {
      val width = buffer.getWidth
      val height = buffer.getHeight

      val rgb = new Array[Int](width * height)
      buffer.getRGB(0, 0, width, height, rgb, 0, width)


      val red = rgb map argb2red
      val green = rgb map argb2green
      val blue = rgb map argb2blue

      val a = pixels2Channel(rgb, argb2alpha)
      val r = pixels2Channel(rgb, argb2red)
      val g = pixels2Channel(rgb, argb2green)
      val b = pixels2Channel(rgb, argb2blue)

      (a, r, g, b)
   }

   private def pixels2Channel(pixels: Array[Int], operator: (Int) => Int) =
   {
      val channel = pixels map operator
      val ret = new BufferedImage(buffer.getWidth(), buffer.getHeight(), BufferedImage.TYPE_BYTE_GRAY)

      ret.getRaster.setPixels(
         0,
         0,
         buffer.getWidth,
         buffer.getHeight,
         channel)

      new GreyScaleImage(ret)
   }

   def intPixels() =
   {
      val width = buffer.getWidth
      val height = buffer.getHeight

      val rgb = new Array[Int](width * height)
      buffer.getRGB(0, 0, width, height, rgb, 0, width)

      rgb
   }
}

object RGBImage
{
   def apply(alpha: GreyScaleImage,
             red: GreyScaleImage,
             green: GreyScaleImage,
             blue: GreyScaleImage) =
   {
      val ret = new BufferedImage(alpha.width, alpha.height, BufferedImage.TYPE_4BYTE_ABGR)

      val a = alpha.bytePixels
      val r = red.bytePixels
      val g = green.bytePixels
      val b = blue.bytePixels

      val pixels = new Array[Int](4*a.size)

      (0 until a.size).foreach(i => {
         pixels(i*4) = r(i).toInt
         pixels(i*4+1) = g(i).toInt
         pixels(i*4+2) = b(i).toInt
         pixels(i*4+3) = a(i).toInt
      })

      ret.getRaster.setPixels(
         0,
         0,
         alpha.width,
         alpha.height,
         pixels)

      new RGBImage(ret)
   }
}
