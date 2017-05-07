package mgoeminne.images.core.mask

import mgoeminne.images.core.{Canvas, Image}

case class MaskedImage[T <: Image[T,R], R](mask: Mask, image: Image[T,R]) extends Canvas[MaskedImage[T,R], R]
{
   override def horizontalFlip = MaskedImage(mask.horizontalFlip, image.horizontalFlip)

   override def verticalFlip = MaskedImage(mask.verticalFlip, image.verticalFlip)

   override def transpose = MaskedImage(mask.transpose, image.transpose)

   override def rotate90 = MaskedImage(mask.rotate90, image.rotate90)

   override def rotate180 = MaskedImage(mask.rotate180, image.rotate180)

   override def rotate270 = MaskedImage(mask.rotate270, image.rotate270)

   override def cut(x: Int, y: Int, width: Int, height: Int) = MaskedImage(mask.cut(x, y, width, height), image.cut(x, y, width, height))

   override def draw(title: String): Unit = image.maskToValue(Mask.not(mask), image.bgColor)
}
