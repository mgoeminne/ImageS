package mgoeminne.images.test.core

import java.io.File

import mgoeminne.images.core.Image
import org.scalatest.{FlatSpec, Matchers}

class BinaryImageTest extends FlatSpec with Matchers
{
   "Horizontally flipped image" should "be different from the original image" in {
      val origin = Image(new File("demo/images/interblocage.jpg")).asBinary
      val flip = origin.horizontalFlip

      origin should not be flip
   }

   "Vertically flipped image" should "be different from the original image" in
      {
         val origin = Image(new File("demo/images/interblocage.jpg")).asBinary
         val flip = origin.verticalFlip

         origin should not be flip
      }

   "Double horizontally flipped image" should "correspond to the image itself" in
      {
         val origin = Image(new File("demo/images/interblocage.jpg")).asBinary
         val doubleFlip = origin.asBinary.horizontalFlip.horizontalFlip

         origin shouldBe doubleFlip
      }

   "Double vertically flipped image" should "correspond to the image itself" in
      {
         val origin = Image(new File("demo/images/interblocage.jpg")).asBinary
         val doubleFlip = origin.verticalFlip.verticalFlip

         origin shouldBe doubleFlip
      }

}
