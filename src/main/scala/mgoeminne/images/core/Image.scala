package mgoeminne.images.core

import java.awt.image.BufferedImage
import java.io.File
import java.net.URL
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JFrame, JLabel, JPanel}

/**
  * A generic representation of an image.
  *
  * Each type of image has 3 distinct representations:
  *
  * - An internal representation that optimizes memory and CPU consumption. Details about this representation
  * are hidden.
  * - A specific representation, that associates each pixel to a Scala variable, depending on the nature of the pixels.
  *   - Binary image pixels are represented by a [[scala.Boolean]].
  *   - Greyscale image pixels are represented by a [[scala.Byte]], having a value between [[scala.Byte.MinValue]] and [[scala.Byte.MaxValue]].
  *   - RGB image pixels are represented by a [[java.awt.Color]].
  * - A generic representation, that associates each pixel to a [[scala.Int]]. This allows the user to manipulate any kind
  * of pixel, regardless the actual image representation.
  *
  *
  * @tparam T the type of concrete image.
  * @tparam R the type of object used for the specific representation of the pixels contained in this image.
  */
abstract class Image[T <: Image[T,R], R] (val width: Int, val height: Int)
{
   def draw(title: String = ""): Unit =
   {
      val picLabel = new JLabel(new ImageIcon(toBufferedImage))
      val pane = new JPanel()
      pane.add(picLabel)
      val frame = new JFrame(title)
      frame.setContentPane(pane)
      frame.pack()
      frame.setVisible(true)
   }

   /**
     * Flips the image horizontally, so that left pixels correspond to the right pixels, and vice versa.
     * @return A horizontally flipped version of this image.
     */
   def horizontalFlip: T

   /**
     * Flips the image vertically, so that top pixels correspond to the bottom pixels, and vice versa.
     * @return A vertically flipped version of this image.
     */
   def verticalFlip: T

   /**
     * Transposes this image.
     * @return The transpose of this image.
     */
   def transpose: T

   /**
     * Rotates the image by 90° clockwise around its center, with no loss of pixel data.
     * @return this image after a rotation by 90° clockwise.
     */
   def rotate90: T


   /**
     * Rotates the image by 180° around its center, with no loss of pixel data.
     * @return this image after a rotation by 180°.
     */
   def rotate180: T


   /**
     * Rotates the image by 270° clockwise around its center, with no loss of pixel data.
     * @return this image after a rotation by 270° clockwise, or 90° counterclockwise.
     */
   def rotate270: T

   /**
     * Rotates the image by an arbitrary angle clockwise around its center.
     * Since the value of each pixel is approximated, such a rotate may (and typically will)
     * modify the image, in such a way that even a rotation of 360° doesn't correspond exactly
     * to the original image.
     * @param angle the rotation angle, in degrees.
     * @return this image after a rotation of the specified angle, clockwise.
     */
   def rotate(angle: Float, default: R): T

   /**
     * @return the relative histogram of this image.
     */
   def histogram: Map[R,Float]

   /**
     * Generates a BufferedImage based on this image.
     * @return a BufferedImage representing this image.
     */
   def toBufferedImage: BufferedImage

   protected def singleBuffer: Array[R]

   /**
     * Transforms each pixel of this image into a binary pixel, according to a predicate.
     * @param predicate The predicate used to discriminate the pixels.
     * @return A binary image in which each pixel is black if the predicate is verified for the corresponding pixel
     *         of this image, or white otherwise.
     */
   def binarize(predicate: R => Boolean): BinaryImage = new BinaryImage(singleBuffer.map(predicate), width)

   protected def asInt(alpha: Byte, r: Byte, g: Byte, b: Byte): Int =
      ((alpha.toInt - Byte.MinValue) << 24) |
      ((r.toInt - Byte.MinValue) << 0) |
      ((g.toInt - Byte.MinValue) << 8) |
      ((b.toInt - Byte.MinValue) << 16)
}