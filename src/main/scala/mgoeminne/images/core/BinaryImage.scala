package mgoeminne.images.core

import java.awt.image.{BufferedImage, DataBufferByte}

/**
  * Binary coded image.
  */
class BinaryImage(buffer: BufferedImage) extends Image[BinaryImage](buffer)
{
   override def asBinary() = this

   /**
     * Transforms the binary image into an array of integers representing it.
     * Each value is 1 for true, and 0 for false.
     *
     * @return an array of integers representing the binary image;
     */
   def integerPixels = buffer.getRaster.getPixels(0,0, width, height, new Array[Int](width*height))

   override def horizontalFlip: BinaryImage =
   {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
      ret.getRaster.setPixels(0, 0, width, height, integerPixels.grouped(width).map(_.reverse).toArray.flatten)

      new BinaryImage(ret)
   }

   /**
     * Vertically flips this image, so that top pixels correspond to the bottom pixels, and vice versa.
     *
     * @return An horizontally flipped version of this image.
     */
   override def verticalFlip: BinaryImage =
   {
      val ret = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)
      ret.getRaster.setPixels(0, 0, width, height, integerPixels.grouped(width).toArray.reverse.flatten)

      new BinaryImage(ret)
   }

   override def equals(that: Any) = {
      that match {
         case x: BinaryImage => this.integerPixels.deep == x.integerPixels.deep
         case _ => false
      }
   }

   override def rotate90: BinaryImage = ???

   override def rotate180: BinaryImage = ???

   override def rotate270: BinaryImage = ???
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
