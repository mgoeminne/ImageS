package mgoeminne.images.core

import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel, JPanel}

import mgoeminne.images.core.mask.Mask

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
abstract class Image[T <: Image[T,R], R] (val width: Int, val height: Int) extends Canvas[T,R]
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
     * @return the relative histogram of this image.
     */
   def histogram: Map[R,Float]

   /**
     * Generates a BufferedImage based on this image.
     * @return a BufferedImage representing this image.
     */
   def toBufferedImage: BufferedImage

   /**
     * Reverses the colors of the image.
     * @return The reversed image.
     */
   def reverse: T

   protected def singleBuffer: Array[R]

   /**
     * Transforms each pixel of this image into a binary pixel, according to a predicate.
     * @param predicate The predicate used to discriminate the pixels.
     * @return A binary image in which each pixel is black if the predicate is verified for the corresponding pixel
     *         of this image, or white otherwise.
     */
   def binarize(predicate: R => Boolean): BinaryImage = new BinaryImage(singleBuffer.map(predicate), width)

   /**
     * Rotates the canvas by an arbitrary angle clockwise around its center.
     * Since the value of each pixel is approximated, such a rotate may (and typically will)
     * modify the canvas, in such a way that even a rotation of 360° doesn't correspond exactly
     * to the original canvas.
     * @param angle the rotation angle, in degrees.
     * @param default the value to give to pixels that "appear" due to the rotation.
     * @return this canvas after a rotation of the specified angle, clockwise.
     */
   def rotate(angle: Float, default: R): T

   protected def asInt(alpha: Byte, r: Byte, g: Byte, b: Byte): Int =
      ((alpha.toInt - Byte.MinValue) << 24) |
      ((r.toInt - Byte.MinValue) << 0) |
      ((g.toInt - Byte.MinValue) << 8) |
      ((b.toInt - Byte.MinValue) << 16)

   /**
     * Applies a mask on this image for produced a masked image.
     * Masked and unmasked pixels in a masked image can be transformed differently.
     * @param mask The mask to apply.
     * @return The resulting masked image.
     */
   //def apply(mask: Mask) = MaskedImage(this, mask)

   /**
     * Changes the value of the masked pixels for the specified value.
     * @param mask  The mask to apply to the image.
     * @param value The new value of the masked pixel.
     * @return      The image, in which masked pixels have been replaced by specified value.
     */
   def maskToValue(mask: Mask, value: R): T
}