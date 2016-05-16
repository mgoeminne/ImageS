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
abstract class Image[T <: Image[T,R], R] (buffer: BufferedImage)
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
     * Transposes this images.
     * @return The transpose of this image.
     */
   def transpose: T = {
      val w = width
      val matrix = intPixels.grouped(w).toArray
      val h = height

      val ret = Array.fill(w,h)(0)

      (0 until h).foreach(j => {
         (0 until w).foreach(i => {
            ret(i)(j) = matrix(j)(i)
         })
      })

      makeImage(ret.flatten, h, w)
   }

   /**
     * Rotates the image by 90° clockwise around its center, with no loss of pixel data.
     * @return this image after a rotation by 90° clockwise.
     */
   def rotate90: T = {
      val w = width
      val matrix = intPixels
      val h = height

      val ret = Array.fill(w*h)(0)

      (0 until h).foreach(j => {
         (0 until w).foreach(i => {
            ret(i*h + h-1-j) = matrix(j*w + i)
         })
      })

      makeImage(ret, h, w)
   }


   /**
     * Rotates the image by 180° around its center, with no loss of pixel data.
     * @return this image after a rotation by 180°.
     */
   def rotate180: T =
   {
      val w = width
      val matrix = intPixels
      val h = height

      val ret = Array.fill(w*h)(0)

      (0 until h).foreach(j => {
         (0 until w).foreach(i => {
            ret((h-1-j)*w + (w-1-i)) = matrix(j*w + i)
         })
      })

      makeImage(ret, w, h)
   }

   /**
     * Rotates the image by 270° clockwise around its center, with no loss of pixel data.
     * @return this image after a rotation by 270° clockwise, or 90° counterclockwise.
     */
   def rotate270: T =
   {
      val w = width
      val h = height
      val matrix = intPixels

      val ret = Array.fill(w*h)(0)

      (0 until h).foreach(j => {
         (0 until w).foreach(i => {
            ret((w-1-i)*h + j) = matrix(j*w + i)
         })
      })

      makeImage(ret, h, w)
   }

   /**
     * Rotates the image by an arbitrary angle clockwise around its center.
     * Since the value of each pixel is approximated, such a rotate may (and typically will)
     * modify the image, in such a way that even a rotation of 360° doesn't correspond exactly
     * to the original image.
     * @param angle the rotation angle, in degrees.
     * @return this image after a rotation of the specified angle, clockwise.
     */
   def rotate(angle: Float, default: R): T =
   {
      val w = width
      val h = height
      val matrix = intPixels
      val angle_radians = Math.toRadians(angle % 360)
      val default_int = asInt(default)

      /**
        * Transforms a cartesian position into a polar position.
        *
        * The polar system has a pole O that corresponds to (0,0) in the cartesian system, and
        * the polaris axis corresponds to the X axis in the cartesian system.
        *
        * A positive angular coordinate means that the angle ϕ is measured counterclockwise from the axis.
        *
        * @param x the x coordinate of the position
        * @param y the y coordinate of the position
        * @return the same position expressed in polar coordinates (radius ρ, angle ϕ).
        */
      def polar(x: Float, y: Float): (Float, Float) =
      {
         val rho = Math.sqrt(x*x + y*y)
         val theta = Math.atan2(y, x)

         (rho.toFloat, theta.toFloat)
      }

      /**
        * Transforms a polar position into a cartesian position.
        * @param rho the radius of the position
        * @param theta the angle of the position, in radians
        * @return the coordinate of the position, in cartesian coordinates.
        */
      def cartesian(rho: Float, theta: Double): (Float, Float) =
      {
         val x = rho * Math.cos(theta)
         val y = rho * Math.sin(theta)

         (x.toFloat, y.toFloat)
      }

      def rotate(x: Int, y: Int, width: Int, height: Int, angle: Double): (Float, Float) =
      {
         val a = x - (width/2)
         val b = y - (height/2)

         polar(a, b) match {
            case (rho: Float, theta: Float) => cartesian(rho, theta - angle_radians) match {
               case (c: Float, d: Float) => (c + (width/2) , d + (height/2))
            }
         }
      }

      val tl = rotate(0,0, w, h, angle_radians)
      val bl = rotate(0,h-1, w, h, angle_radians)
      val tr = rotate(w,0, w, h, angle_radians)
      val br = rotate(w,h-1, w, h, angle_radians)

      val x_list = List(tl._1, bl._1, tr._1, br._1)
      val y_list = List(tl._2, bl._2, tr._2, br._2)

      val x_min = x_list.min
      val x_max = x_list.max
      val y_min = y_list.min
      val y_max = y_list.max

      val a_width = Math.ceil(x_max - x_min).toInt
      val a_height = Math.ceil(y_max - y_min).toInt

      val array = Array.fill[Int](a_width*a_height)(0)

      (0 until a_height).foreach(j => {
         (0 until a_width).foreach(i => {
            val pos = rotate(i, j, a_width, a_height, -angle_radians)
            val x = pos._1+x_min
            val y = pos._2+y_min

            val orig_x = Math.round(x)
            val orig_y = Math.round(y)

            array(j*a_width + i) = if(orig_x >= 0 && orig_x < w && orig_y >= 0 && orig_y < h) matrix(orig_y*w + orig_x)
                                   else default_int
         })
      })

      this.makeImage(array, a_width, a_height)
   }

   protected def asInt(value: R): Int
}

object Image
{
   def apply(source: File) = new RGBImage(ImageIO.read(source))
   def apply(url: URL) = new RGBImage(ImageIO.read(url))
}