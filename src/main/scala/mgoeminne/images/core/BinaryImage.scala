package mgoeminne.images.core

import java.awt.image.{BufferedImage}

/**
  * Binary coded image.
  */
class BinaryImage(buffer: Array[Boolean], width: Int) extends SingleLayerImage[BinaryImage, Boolean](buffer, width, buffer.size / width)
{
   def toBufferedImage: _root_.java.awt.image.BufferedImage = {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
      ret.getRaster.setPixels(0, 0, width, height, buffer.map(pixel => if(pixel) 0 else 1))

      ret
   }

   def toGreyImage = new GreyScaleImage(buffer.map(pixel => if(pixel) Byte.MinValue else Byte.MaxValue), width)

   /**
     * Reverses the colors of this image: white becomes black, and black becomes white.
     * @return The reversed version of this image.
     */
   def reverse: BinaryImage = new BinaryImage(buffer.map(pixel => !pixel), width)

   override protected def makeImage(buffer: Array[Boolean], width: Int): BinaryImage = new BinaryImage(buffer, width)

   override def bgColor: Boolean = false
}

object BinaryImage{

   /**
     * Creates a binary image in which each pixel is set to false.
     * @param width  The image width.
     * @param height The image height.
     * @return A binary image of size (width * height) in which each pixel is set to false.
     */
   def falseImage(width: Int, height: Int) = new BinaryImage(Array.fill(width*height)(false), width)

   /**
     * Creates a binary image in which each pixel is set to true.
     * @param width  The image width.
     * @param height The image height.
     * @return A binary image of size (width * height) in which each pixel is set to true.
     */
   def trueImage(width: Int, height: Int) = new BinaryImage(Array.fill(width*height)(true), width)
}