package mgoeminne.images

import java.awt.Color

/**
  * This package contains the main structures and functions that relate to image processing.
  */
package object core
{
   /**
     * Determines a color between two given colors, according to a linear interpolation
     * of each of their components (A,R,G,B).
     * @param a a color.
     * @param b an other color.
     * @param p the gradient of the interpolation. Must be a value between 0 and 1.
     * @return the color corresponding to the linear interpolation of a and b, according to the following formula:
     *
     *         ret = a * alpha + b * (1-alpha)
     */
   def linear_interpolation(a: Color, b: Color, p: Float): Color =
   {
      val array_a = a.getRGBColorComponents(new Array[Float](4))
      val array_b = b.getRGBColorComponents(new Array[Float](4))

      val red = array_a(0) * p + array_b(0) * (1-p)
      val green = array_a(1) * p + array_b(1) * (1-p)
      val blue = array_a(2) * p + array_b(2) * (1-p)
      val transparency = array_a(3) * p + array_b(3) * (1-p)

      new Color(red, green, blue, transparency)
   }
}
