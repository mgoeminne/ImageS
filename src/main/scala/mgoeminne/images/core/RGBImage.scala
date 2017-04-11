package mgoeminne.images.core

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.net.URL
import javax.imageio.ImageIO

import mgoeminne.images.core.mask.Mask

import scala.Byte.MinValue

/**
  * A ARGB based image.
  */
case class RGBImage(    r: GreyScaleImage,
                        g: GreyScaleImage,
                        b: GreyScaleImage) extends MultiLayerImage[RGBImage, GreyScaleImage, (Byte, Byte, Byte), Byte](r.width, r.height)
{
   override def toBufferedImage() =
   {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

      val pixels = (r.buffer,g.buffer,b.buffer) .zipped
                                                .toArray
                                                .flatMap(p => Array(p._1.toInt - MinValue, p._2.toInt - MinValue, p._3.toInt - MinValue))

      ret.getRaster.setPixels(0, 0, width, height, pixels)
      ret
   }





   /**
     * Rotates the image by an arbitrary angle clockwise around its center.
     * Since the value of each pixel is approximated, such a rotate may (and typically will)
     * modify the image, in such a way that even a rotation of 360° doesn't correspond exactly
     * to the original image.
     *
     * @param angle the rotation angle, in degrees.
     * @return this image after a rotation of the specified angle, clockwise.
     */
   override def rotate(angle: Float, default: (Byte, Byte, Byte)): RGBImage = new RGBImage(
      r.rotate(angle, default._1),
      g.rotate(angle, default._2),
      b.rotate(angle, default._3)
   )


   /**
     * @return the relative histogram of this image.
     */
   override def histogram: Map[(Byte, Byte, Byte), Float] =
   (r.buffer, g.buffer, b.buffer)
      .zipped
      .map { case (r: Byte, g: Byte, b: Byte) => (r,g,b)}
      .groupBy(identity).mapValues(value => value.size / r.buffer.size.toFloat)


   def toARGB(alpha: Byte) = new ARGBImage(new GreyScaleImage(Array.fill[Byte](r.buffer.size)(alpha), r.width), r, g, b)


   def toARGB(alpha: GreyScaleImage) = new ARGBImage(alpha, r, g, b)


   def asGreyScaleImage = new GreyScaleImage( (r.buffer, g.buffer, b.buffer).zipped
                                                                            .map { case (r,g,b) => ((r.toInt+b.toInt+b.toInt) / 3).toByte } , width)


   override protected def singleBuffer: Array[(Byte, Byte, Byte)] =
      (0 until r.buffer.size).map(i => (r.buffer(i), g.buffer(i), b.buffer(i))).toArray


   override def equals(other: Any) = other match {
      case x: RGBImage => (x.r == this.r) && (x.g == this.g) && (x.b == this.b)
   }


   /**
     * @return the standard luminance of the image.
     */
   def luminance = new GreyScaleImage(

      (r.buffer, g.buffer, b.buffer).zipped.map {
         case (r: Byte, g: Byte, b: Byte) => {
            val red = ((r.toInt - Byte.MinValue) * 0.2126)
            val green = ((g.toInt - Byte.MinValue) * 0.7152)
            val blue = ((b.toInt - Byte.MinValue) * 0.0722)

            (((red + green + blue) / 3) + Byte.MinValue).toByte
         }
      },
      width
   )

   override def maskToValue(mask: Mask, value: (Byte, Byte, Byte)) = {
      new RGBImage(
         r.maskToValue(mask, value._1),
         g.maskToValue(mask, value._2),
         b.maskToValue(mask, value._3)
      )
   }

   override def applyOnEachLayer(f: (GreyScaleImage) => GreyScaleImage): RGBImage = new RGBImage(f(r), f(g), f(b))
}

object RGBImage
{
   def apply(source: File): RGBImage = apply(ImageIO.read(source))


   def apply(url: URL): RGBImage = apply(ImageIO.read(url))


   def apply(source: BufferedImage): RGBImage =
   {
      val width = source.getWidth
      val height = source.getHeight

      val rgb = new Array[Int](3*width*height)
      source.getRaster.getPixels(0, 0, width, height, rgb)

      val pixels = rgb.grouped(3).toArray

      val red = pixels map (p => ((p(0)+MinValue).toByte))
      val green = pixels map (p => ((p(1)+MinValue).toByte))
      val blue = pixels map (p => ((p(2)+MinValue).toByte))

      new RGBImage(
         new GreyScaleImage(red, width),
         new GreyScaleImage(green, width),
         new GreyScaleImage(blue, width)
      )
   }

   def apply(width: Int, height: Int, r: Byte, g:Byte, b:Byte) = new RGBImage(
      GreyScaleImage(width, height, r),
      GreyScaleImage(width, height, g),
      GreyScaleImage(width, height, b)
   )
}