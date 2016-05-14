package mgoeminne.images.test.core

import java.io.File

import mgoeminne.images.core.{Image, RGBImage}
import org.scalatest.{FlatSpec, Matchers}

class RGBImageTest extends FlatSpec with Matchers
{
   "A RGB image" should "be correctly decomposed into A,R,G,B channels" in
   {
      val origin = Image(new File("demo/images/interblocage.jpg"))

      val channels = origin.decompose
      val recomposed = RGBImage(channels._1, channels._2, channels._3, channels._4)

      origin shouldBe recomposed
   }

   "Horizontally flipped image" should "be different from the original image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val flip = origin.horizontalFlip

      origin should not be flip
   }

   "Vertically flipped image" should "be different from the original image" in
   {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val flip = origin.verticalFlip

      origin should not be flip
   }

   "Double horizontally flipped image" should "correspond to the original image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val flip = origin.horizontalFlip.horizontalFlip

      origin shouldBe flip
   }

   "Double vertically flipped image" should "correspond to the image itself" in
   {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val doubleFlip = origin.verticalFlip.verticalFlip

      origin shouldBe doubleFlip
   }

   "A rotation by 90°" should "alter the image" in {
      val origin = Image(new File("demo/images/interblocage.jpg")).asBinary
      val rot = origin.rotate90

      origin should not be rot
   }

   "A quadruple rotation by 90°" should "give the initial image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val rot = origin.rotate90.rotate90.rotate90.rotate90

      origin shouldBe rot
   }

   "A rotation by 180°" should "alter the image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val rot = origin.rotate180

      origin should not be rot
   }

   "A double rotation by 180°" should "give the initial image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val rot = origin.rotate180.rotate180

      origin shouldBe rot
   }

   "A rotation by 270°" should "alter the image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val rot = origin.rotate270

      origin should not be rot
   }

   "A quadruple rotation by 270°" should "give the initial image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val rot = origin.rotate270.rotate270.rotate270.rotate270

      origin shouldBe rot
   }

   "A rotation by 90° + 270°" should "give the initial image" in {
      val origin = Image(new File("demo/images/interblocage.jpg"))
      val rot = origin.rotate90.rotate270

      origin shouldBe rot
   }
}
