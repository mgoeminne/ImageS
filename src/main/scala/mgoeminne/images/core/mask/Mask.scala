package mgoeminne.images.core.mask

import java.awt.Polygon

import mgoeminne.images.core.{BinaryImage, Canvas, GreyScaleImage, Image}

import scala.collection.mutable.ArrayBuffer


/**
  * A mask for an arbitrary image.
  *
  * A mask represents a selection of pixels among the pixels belonging to
  * the underlying image.
  */
case class Mask(selection: BinaryImage) extends Canvas[Mask, Boolean]
{
   /**
     * Flips the mask horizontally, so that left pixels correspond to the right pixels, and vice versa.
     *
     * @return A horizontally flipped version of this mask.
     */
   def horizontalFlip = Mask(selection.horizontalFlip)

   /**
     * Flips the mask vertically, so that top pixels correspond to the bottom pixels, and vice versa.
     * @return A vertically flipped version of this mask.
     */
   def verticalFlip = Mask(selection.verticalFlip)

   /**
     * Transposes this mask.
     * @return The transpose of this mask.
     */
   def transpose = Mask(selection.transpose)

   /**
     * Rotates the mask by 90° clockwise around its center, with no loss of pixel data.
     * @return this mask after a rotation by 90° clockwise.
     */
   def rotate90 = Mask(selection.rotate90)


   /**
     * Rotates the mask by 180° around its center, with no loss of pixel data.
     * @return this mask after a rotation by 180°.
     */
   def rotate180 = Mask(selection.rotate180)


   /**
     * Rotates the mask by 270° clockwise around its center, with no loss of pixel data.
     * @return this mask after a rotation by 270° clockwise, or 90° counterclockwise.
     */
   def rotate270 = Mask(selection.rotate270)

   /**
     * Rotates the mask by an arbitrary angle clockwise around its center.
     * Since the value of each pixel is approximated, such a rotate may (and typically will)
     * modify the mask, in such a way that even a rotation of 360° doesn't correspond exactly
     * to the original mask.
     * @param angle the rotation angle, in degrees.
     * @return this mask after a rotation of the specified angle, clockwise.
     */
   def rotate(angle: Float) = Mask(selection.rotate(angle, false))

   private def combine(other: Mask, f: (Boolean, Boolean) => Boolean) = {
      Mask(new BinaryImage((this.selection.buffer zip other.selection.buffer) map { case (a,b) => f(a,b) }, selection.width))

   }

   /**
     * @param other An other mask.
     * @return The intersection of this mask and the other mask.
     */
   def intersect(other: Mask) = combine(other, {(a: Boolean, b: Boolean) => a && b})

   /**
     * @param other An other mask.
     * @return The intersection of this mask and the other mask.
     */
   def union(other: Mask) = combine(other, {(a: Boolean, b: Boolean) => a || b})

   override def draw(title: String): Unit = ???

   /**
     * Cuts an image by selecting a a rectangle included in the canvas of the current image.
     *
     * @param x      The abscisse of the top-left corner of the sub-rectangle.
     * @param y      The ordinate of the top-left corner of the sub-rectangle.
     * @param width  The width of the sub-rectangle.
     * @param height The height of the sub-rectangle.
     */
   override def cut(x: Int, y: Int, width: Int, height: Int): Mask = Mask(selection.cut(x, y, width, height))
}

object Mask{
   /**
     * Creates a rectangular boolean mask.
     * @param width  The mask of the image to be masked.
     * @param height The height of the image to be masked.
     * @param x1     The abscissa of the top-left corner of the mask.
     * @param y1     The ordinate of the top-left corner of the mask.
     * @param x2     The abscissa of the top-left corner of the mask.
     * @param y2     The ordinate of the top-left corner of the mask.
     * @return       The mask corresponding to the specified rectangle.
     */
   def rectangle(width: Int, height: Int, x1: Int, y1: Int, x2: Int, y2: Int): Mask = {

      if (x1 > x2) rectangle(width, height, x2, x1, y2, y1)
      if (y1 > y2) rectangle(width, height, x1, x2, y1, y2)

      val buffer = (0 until height).map(j => {
         if (j < y1 || j > y2) Array.fill(width)(false)
         else {
            val prefix = if(x1==0) None
                         else Some(Array.fill(x1-1)(false))
            val suffix = if(x2 >= width-1) None
                         else Some(Array.fill(width-x2)(false))
            val core = if(x1 >= x2) None
                       else Some(Array.fill(x2 - x1 + 1)(true))
            List(prefix, core, suffix).flatten.flatten.toArray
         }
      }).flatten.toArray

      new Mask(new BinaryImage(buffer, width))
   }

   def polygon(width: Int, height: Int, points: Seq[(Int, Int)]): Mask = {
      val buffer = new ArrayBuffer[Boolean](width*height)
      val poly = new Polygon(points.map(_._1).toArray, points.map(_._2).toArray, points.size)

      (0 until height).map(j => {
         (0 until width).map(i => {
            buffer.append(poly contains(i, j))
         })
      })

      new Mask(new BinaryImage(buffer.toArray, width))
   }

   def not(mask: Mask) = new Mask(new BinaryImage(mask.selection.buffer.map(!_), mask.selection.width))
}
