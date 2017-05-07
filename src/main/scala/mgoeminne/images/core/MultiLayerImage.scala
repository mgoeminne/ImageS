package mgoeminne.images.core

import scala.reflect.ClassTag


/**
  * This class is implemented by image classes based on multiple layers.
  */
abstract class MultiLayerImage[C <: MultiLayerImage[C,T,R,S], T <: SingleLayerImage[T,S], R: ClassTag, S: ClassTag](width: Int, height: Int) extends Image[C,R](width, height)
{
   /**
     * Applies the specified transformation to each underlying layer of this image.
     * @param f The transformation to apply to each underlying layer of this image.
     * @return The resulting image obtained by applying f to each underlying layer.
     */
   def applyOnEachLayer(f: T => T): C

   final override def reverse = this.applyOnEachLayer(x => x.reverse)

   final override def horizontalFlip = this.applyOnEachLayer(_.horizontalFlip)

   final override def verticalFlip = this.applyOnEachLayer(_.verticalFlip)

   final override def transpose = this.applyOnEachLayer(_.transpose)

   final override def rotate90 = this.applyOnEachLayer(_.rotate90)

   final override def rotate180 = this.applyOnEachLayer(_.rotate180)

   final override def rotate270 = this.applyOnEachLayer(_.rotate270)

   final override def cut(x: Int, y: Int, width: Int, height: Int) = this.applyOnEachLayer(_.cut(x, y, width, height))
}
