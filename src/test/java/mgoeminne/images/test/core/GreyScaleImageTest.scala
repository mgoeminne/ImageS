package mgoeminne.images.test.core

import java.io.File

import mgoeminne.images.core.{Image, RGBImage}
import org.scalatest.{FlatSpec, Matchers}

class GreyScaleImageTest extends FlatSpec with Matchers
{
   "Horizontally flipped image" should "be different from the original image" in {
      val origin = Image(new File("demo/images/interblocage.jpg")).asGreyScale
      val flip = origin.horizontalFlip

      origin should not be flip
   }

   "Vertically flipped image" should "be different from the original image" in
      {
         val origin = Image(new File("demo/images/interblocage.jpg")).asGreyScale
         val flip = origin.verticalFlip

         origin should not be flip
      }

   "Double horizontally flipped image" should "correspond to the image itself" in
      {
         val origin = Image(new File("demo/images/interblocage.jpg")).asGreyScale()
         val doubleFlip = origin.horizontalFlip.horizontalFlip

         origin shouldBe doubleFlip
      }

   "Double vertically flipped image" should "correspond to the image itself" in
      {
         val origin = Image(new File("demo/images/interblocage.jpg")).asGreyScale()
         val doubleFlip = origin.verticalFlip.verticalFlip

         origin shouldBe doubleFlip
      }
}
