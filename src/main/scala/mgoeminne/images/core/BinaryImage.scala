package mgoeminne.images.core

import java.awt.image.BufferedImage

/**
  * Binary coded image.
  */
class BinaryImage(buffer: BufferedImage) extends Image(buffer)
{
   override def asBinary() = this
}
