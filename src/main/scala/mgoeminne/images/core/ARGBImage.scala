package mgoeminne.images.core

import java.awt.image.BufferedImage
import java.io.File
import java.net.URL
import javax.imageio.ImageIO

import mgoeminne.images.core.mask.Mask

/**
  * An image represented by four channels:
  *   - alpha, the transparency.
  *   - r, g, and b, the basic colour channels.
  */
class ARGBImage(val a: GreyScaleImage,
                val r: GreyScaleImage,
                val g: GreyScaleImage,
                val b: GreyScaleImage
               ) extends MultiLayerImage[ARGBImage, GreyScaleImage, (Byte, Byte, Byte, Byte), Byte](a.width, a.height)
{
   override def toBufferedImage() =
   {
      def asInt(alpha: Byte, r: Byte, g: Byte, b: Byte) = (((alpha.toInt+Byte.MinValue) & 0xff) << 24) |
                                                          (((r.toInt+Byte.MinValue) & 0xff) << 16) |
                                                          (((g.toInt+Byte.MinValue) & 0xff) << 8) |
                                                          (((b.toInt+Byte.MinValue) & 0xff) << 0)


      println("aaa")

      val ret = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

      println(singleBuffer.map{ case (a,r,g,b) => asInt(a,r,g,b)}.mkString)

      val t = singleBuffer.map{ case (a,r,g,b) => asInt(a,r,g,b)}

      println(t.size)

      ret.getRaster.setPixel(0, 0, t)

      println(ret.getWidth)
      println(ret.getHeight)


      ret
   }

   /**
     * Transforms the image into a RGB image by removing the alpha channel, which is definitively lost.
     *
     * @return a RGB version of the image.
     */
   def asRGB(): RGBImage = new RGBImage(r,g,b)

   /**
     * Transforms the image into a RGB image, by replacing the alpha according to a given background color.
     * The resulting image will consider that this image is placed on the top of a uniform background image.
     * @param background The background color to use.
     * @return
     */
   def asRGB(background: (Byte, Byte, Byte)): RGBImage = {

      def blend(original: GreyScaleImage, alpha: GreyScaleImage, background: Byte): GreyScaleImage =
         new GreyScaleImage(original.buffer.zip(alpha.buffer).map { case (o,a) => ((1-((a-Byte.MinValue) / Byte.MaxValue.toFloat))*o + (a*background)).toByte}, r.width)

      new RGBImage(
         blend(r, a, background._1),
         blend(g, a, background._2),
         blend(b, a, background._3)
      )
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
   override def rotate(angle: Float, default: (Byte, Byte, Byte, Byte)): ARGBImage = new ARGBImage(
      a.rotate(angle, default._1),
      r.rotate(angle, default._2),
      g.rotate(angle, default._3),
      b.rotate(angle, default._4)
   )

   /**
     * @return the relative histogram of this image.
     */
   override def histogram: Map[(Byte, Byte, Byte, Byte), Float] = singleBuffer.groupBy(identity).mapValues(_.size)

   override protected def singleBuffer: Array[(Byte, Byte, Byte, Byte)] =
      (0 until a.buffer.size).map(i => (a.buffer(i), r.buffer(i), g.buffer(i), b.buffer(i))).toArray

   override def maskToValue(mask: Mask, value: (Byte, Byte, Byte, Byte)) = {
      new ARGBImage(
         a.maskToValue(mask, value._1),
         r.maskToValue(mask, value._2),
         g.maskToValue(mask, value._3),
         b.maskToValue(mask, value._4)
      )
   }

   override def applyOnEachLayer(f: (GreyScaleImage) => GreyScaleImage): ARGBImage = new ARGBImage(f(a), f(r), f(b), f(g))

   override def bgColor: (Byte, Byte, Byte, Byte) = (0,0,0, Byte.MinValue)
}
