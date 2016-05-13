package mgoeminne.images.core

import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.File
import java.net.URL
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JFrame, JLabel, JPanel}

/**
  * A generic representation of an image.
  */
abstract class Image[T <: Image[T]] (buffer: BufferedImage)
{
   def draw(title: String = ""): Unit =
   {
      val picLabel = new JLabel(new ImageIcon(buffer))
      val pane = new JPanel()
      pane.add(picLabel);
      val frame = new JFrame(title)
      frame.setContentPane(pane)
      frame.pack()
      frame.setVisible(true)
   }

   /**
     * Creates a new instance of the same type that this image, with the specified
     * pixels, width and height.
     *
     * This method uses a Factory Method Pattern (GoF) for generalizing the implementation of generic
     * pixel transformations.
     *
     * @param pixels an array of integers representing the pixels
     * @param width the image width
     * @param height the image height
     * @return a new image having the specified pixels, width and height.
     */
   protected def makeImage(pixels: Array[Int], width: Int, height: Int): T

   /**
     * Transforms the image into an array of integers representing it.
     * Each value is 1 for true, and 0 for false.
     *
     * @return an array of integers representing the image
     */
   protected def intPixels = buffer.getRaster.getPixels(0,0, width, height, new Array[Int](width*height))

   /**
     * Flips the image horizontally, so that left pixels correspond to the right pixels, and vice versa.
     * @return A horizontally flipped version of this image.
     */
   def horizontalFlip: T = makeImage(intPixels.grouped(width).map(_.reverse).toArray.flatten, width, height)

   /**
     * Flips the image vertically, so that top pixels correspond to the bottom pixels, and vice versa.
     * @return A vertically flipped version of this image.
     */
   def verticalFlip: T = makeImage(intPixels.grouped(width).toArray.reverse.flatten, width, height)

   /**
     * Transforms the image into a greyscale image.
     * @return A greyscale version of the image.
     */
   def asGreyScale(): GreyScaleImage = new GreyScaleImage(this.changeColorPalette(BufferedImage.TYPE_BYTE_GRAY))

   /**
     * Transforms the image into a black and white image.
     * @return A binary (black and white) version of the image.
     */
   def asBinary(): BinaryImage = new BinaryImage(this.changeColorPalette(BufferedImage.TYPE_BYTE_BINARY))

   def asRGB(): RGBImage = new RGBImage(this.changeColorPalette(BufferedImage.TYPE_4BYTE_ABGR))

   private def changeColorPalette(colorPalette: Int) =
   {
      val other = new BufferedImage(width, height, colorPalette)

      val g = other.createGraphics()
      g.drawImage(buffer, 0, 0, null)

      other
   }

   override def hashCode = this.buffer.hashCode

   def width: Int = this.buffer.getData.getWidth
   def height: Int = this.buffer.getData.getHeight


   /**
     * Rotates the image by 90° clockwise around its center, with no loss of pixel data.
     * @return this image after a rotation by 90° clockwise.
     */
   def rotate90: T = ???


   /**
     * Rotates the image by 180° around its center, with no loss of pixel data.
     * @return this image after a rotation by 180°.
     */
   def rotate180: T = ???

   /**
     * Rotates the image by 270° clockwise around its center, with no loss of pixel data.
     * @return this image after a rotation by 270° clockwise, or 90° counterclockwise.
     */
   def rotate270: T = ???
}

object Image
{
   def apply(source: File) = new RGBImage(ImageIO.read(source))
   def apply(url: URL) = new RGBImage(ImageIO.read(url))
}