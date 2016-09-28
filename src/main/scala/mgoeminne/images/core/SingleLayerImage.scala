package mgoeminne.images.core

import scala.reflect.ClassTag

/**
  * Created by mg on 24/09/16.
  */
abstract class SingleLayerImage[T <: Image[T,R] : ClassTag, R: ClassTag](val buffer: Array[R], width: Int, height: Int) extends Image[T,R](width, height)
{
   def horizontalFlip = {
      val data = buffer.grouped(width).map(_.reverse)
      makeImage(data.toArray.flatten, width)
   }

   def verticalFlip: T = makeImage(buffer.grouped(width).toArray.reverse.flatten, width)

   override def hashCode = this.buffer.hashCode

   def transpose = {
      val matrix = buffer.grouped(width).toArray

      val ret = new Array[R](width*height)

      (0 until height).foreach(j => {
         (0 until width).foreach(i => {
            ret(i*width + j) = matrix(j)(i)
         })
      })

      makeImage(ret, height)
   }

   protected def makeImage(buffer: Array[R], width: Int): T

   def rotate90: T = {

      val ret: Array[R] = new Array[R](width*height)

      (0 until height).foreach(j => {
         (0 until width).foreach(i => {
            ret(i*height + height-1-j) = buffer(j*width + i)
         })
      })

      makeImage(ret, height)
   }

   def rotate180 = {
      val ret = new Array[R](width*height)

      (0 until height).foreach(j => {
         (0 until width).foreach(i => {
            ret((height-1-j)*width + (width-1-i)) = buffer(j*width + i)
         })
      })

      makeImage(ret, width)
   }

   def rotate270: T =
   {
     val ret = new Array[R](width*height)

      (0 until height).foreach(j => {
         (0 until width).foreach(i => {
            ret((width-1-i)*height + j) = buffer(j*width + i)
         })
      })

      makeImage(ret, height)
   }

   def rotate(angle: Float, default: R): T = {
      val w = width
      val h = height
      val angle_radians = Math.toRadians(angle % 360)

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

      val array = new Array[R](a_width * a_height)

      (0 until a_height).foreach(j => {
         (0 until a_width).foreach(i => {
            val pos = rotate(i, j, a_width, a_height, -angle_radians)
            val x = pos._1+x_min
            val y = pos._2+y_min

            val orig_x = Math.round(x)
            val orig_y = Math.round(y)

            array(j*a_width + i) = if(orig_x >= 0 && orig_x < w && orig_y >= 0 && orig_y < h) buffer(orig_y*w + orig_x)
                                   else default
         })
      })

      this.makeImage(array, a_width)
   }

   /**
     * Transforms this image by applying a function to each of its pixels.
     *
     * @param f A function defining the transformation of each of the pixels
     * @return A new image, corresponding to this after the transformation function
     *         has been applied to each of its pixels.
     */
   def transform(f: R => R): T = makeImage((buffer.map(f)), width)

   def histogram = buffer.groupBy(identity).mapValues(v => v.size / buffer.size.toFloat)

   override def singleBuffer: Array[R] = buffer

   override def equals(that: Any) = {
      that match {
         case x: SingleLayerImage[T,R] => this.width == x.width && (this.buffer.deep == x.buffer.deep)
         case _ => false
      }
   }
}
