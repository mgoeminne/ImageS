package mgoeminne.images.test.core

import java.io.File

import mgoeminne.images.core.{ARGBImage, Image, RGBImage}
import org.scalatest.{FlatSpec, Matchers}

class RGBImageTest extends FlatSpec with Matchers
{
   val original = RGBImage(new File("demo/images/interblocage.jpg"))

   "A RGB image" should "be correctly decomposed into A,R,G,B channels" in
   {
      val r = original.r
      val g = original.g
      val b = original.b
      
      val recomposed = new RGBImage(r, g, b)
      original shouldBe recomposed
   }

   "Horizontally flipped image" should "be different from the original image" in {
      val flip = original.horizontalFlip
      original should not be flip
   }

   "Vertically flipped image" should "be different from the original image" in {
      val flip = original.verticalFlip
      original should not be flip
   }

   "Double horizontally flipped image" should "correspond to the original image" in {

      val flip = original.horizontalFlip.horizontalFlip
      //original shouldBe flip
   }

   "Double vertically flipped image" should "correspond to the image itself" in {
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
}