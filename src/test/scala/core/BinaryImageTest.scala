package core

import java.io.File

import mgoeminne.images.core.{ARGBImage, RGBImage}
import org.scalatest.{FlatSpec, Matchers}

class BinaryImageTest extends FlatSpec with Matchers
{
   val original = RGBImage(new File("demo/images/interblocage.jpg"))
      .asGreyScaleImage
      .binarize(x => x >= 0)

   "Horizontally flipped image" should "be different from the original image" in {
      val flip = original.horizontalFlip

      original should not be flip
   }

   "Vertically flipped image" should "be different from the original image" in
      {
         val flip = original.verticalFlip
         original should not be flip
      }

   "Double horizontally flipped image" should "correspond to the image itself" in
      {
         val doubleFlip = original.horizontalFlip.horizontalFlip
         original shouldBe doubleFlip
      }

   "Double vertically flipped image" should "correspond to the image itself" in
      {
         val doubleFlip = original.verticalFlip.verticalFlip
         original shouldBe doubleFlip
      }

   "A rotation by 90°" should "alter the image" in {
      val rot = original.rotate90
      original should not be rot
   }

   "A quadruple rotation by 90°" should "give the initial image" in {
      val rot = original.rotate90.rotate90.rotate90.rotate90
      original shouldBe rot
   }

   "A rotation by 180°" should "alter the image" in {
      val rot = original.rotate180
      original should not be rot
   }

   "A double rotation by 180°" should "give the initial image" in {
      val rot = original.rotate180.rotate180
      original shouldBe rot
   }

   "A rotation by 270°" should "alter the image" in {
      val rot = original.rotate270
      original should not be rot
   }

   "A quadruple rotation by 270°" should "give the initial image" in {
      val rot = original.rotate270.rotate270.rotate270.rotate270
      original shouldBe rot
   }

   "A rotation by 90° + 270°" should "give the initial image" in {
      val rot = original.rotate90.rotate270
      original shouldBe rot
   }

   "The relative histogram of a binary image" should "have a total of 1" in {
      original.histogram.map(_._2).sum.toDouble shouldBe 1.0 +- 0.001
   }

   it should "correspond to the histogram observed in an other image tool" in {
      val hist = original.histogram

      hist(false).toDouble shouldBe 0.646 +- 0.001
      hist(true).toDouble shouldBe 0.354 +- 0.001
   }

   "A reversed image" should "be different from the original image (in general)" in {
      original.reverse should not be original
   }

   "An image reversed twice" should "be the original image" in {
      original.reverse.reverse shouldBe original
   }

   "A cut image" should "have the appropriate dimensions" in {
      original.cut(10, 15, 100, 26).shape shouldBe (100, 26)
   }
}
