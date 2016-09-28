package mgoeminne.images.core

import java.awt.image.BufferedImage
import java.io.File
import java.net.URL
import javax.imageio.ImageIO

/**
  * Created by mg on 25/09/16.
  */
class ARGBImage(val a: GreyScaleImage,
                val r: GreyScaleImage,
                val g: GreyScaleImage,
                val b: GreyScaleImage
               ) extends Image[ARGBImage, (Byte, Byte, Byte, Byte)](a.width, a.height)
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
     * Flips the image horizontally, so that left pixels correspond to the right pixels, and vice versa.
     *
     * @return A horizontally flipped version of this image.
     */
   override def horizontalFlip: ARGBImage = new ARGBImage(
      a.horizontalFlip,
      r.horizontalFlip,
      g.horizontalFlip,
      b.horizontalFlip
   )

   /**
     * Flips the image vertically, so that top pixels correspond to the bottom pixels, and vice versa.
     *
     * @return A vertically flipped version of this image.
     */
   override def verticalFlip: ARGBImage = new ARGBImage(
      a.verticalFlip,
      r.verticalFlip,
      g.verticalFlip,
      b.verticalFlip
   )

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
     * Transposes this image.
     *
     * @return The transpose of this image.
     */
   override def transpose: ARGBImage = new ARGBImage(
      a.transpose,
      r.transpose,
      g.transpose,
      b.transpose
   )

   /**
     * Rotates the image by 90° clockwise around its center, with no loss of pixel data.
     *
     * @return this image after a rotation by 90° clockwise.
     */
   override def rotate90: ARGBImage = new ARGBImage(
      a.rotate90,
      r.rotate90,
      g.rotate90,
      b.rotate90
   )

   /**
     * Rotates the image by 180° around its center, with no loss of pixel data.
     *
     * @return this image after a rotation by 180°.
     */
   override def rotate180: ARGBImage = new ARGBImage(
      a.rotate180,
      r.rotate180,
      g.rotate180,
      b.rotate180
   )

   /**
     * Rotates the image by 270° clockwise around its center, with no loss of pixel data.
     *
     * @return this image after a rotation by 270° clockwise, or 90° counterclockwise.
     */
   override def rotate270: ARGBImage = new ARGBImage(
      a.rotate270,
      r.rotate270,
      g.rotate270,
      b.rotate270
   )

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

   /**
     * Reverses the colors of the image.
     *
     * @return The reversed image.
     */
   override def reverse: ARGBImage = new ARGBImage(a.reverse, r.reverse, g.reverse, b.reverse)
}
