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
      val other = new BufferedImage(buffer.getWidth(), buffer.getHeight(), colorPalette)

      val g = other.createGraphics()
      g.drawImage(buffer, 0, 0, null)

      other
   }

   override def hashCode = this.buffer.hashCode

   def width: Int = this.buffer.getData.getWidth
   def height: Int = this.buffer.getData.getHeight

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
}

object Image
{
   def apply(source: File) = new RGBImage(ImageIO.read(source))
   def apply(url: URL) = new RGBImage(ImageIO.read(url))
}