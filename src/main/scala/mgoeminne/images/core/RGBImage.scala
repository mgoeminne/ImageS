package mgoeminne.images.core

import java.awt.image.BufferedImage

/**
  * A ARGB based image.
  */
case class RGBImage(buffer: BufferedImage) extends Image(buffer)
{
   override def asRGB() = this
}
