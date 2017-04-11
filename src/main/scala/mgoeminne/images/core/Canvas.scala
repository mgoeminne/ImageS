package mgoeminne.images.core

/**
  * Created by Mathieu Goeminne.
  */
trait Canvas[T <: Canvas[T,R], R]
{
   /**
     * Flips the canvas horizontally, so that left pixels correspond to the right pixels, and vice versa.
     * @return A horizontally flipped version of this canvas.
     */
   def horizontalFlip: T

   /**
     * Flips the canvas vertically, so that top pixels correspond to the bottom pixels, and vice versa.
     * @return A vertically flipped version of this canvas.
     */
   def verticalFlip: T

   /**
     * Transposes this canvas.
     * @return The transpose of this canvas.
     */
   def transpose: T

   /**
     * Rotates the canvas by 90° clockwise around its center, with no loss of pixel data.
     * @return this canvas after a rotation by 90° clockwise.
     */
   def rotate90: T


   /**
     * Rotates the canvas by 180° around its center, with no loss of pixel data.
     * @return this canvas after a rotation by 180°.
     */
   def rotate180: T


   /**
     * Rotates the canvas by 270° clockwise around its center, with no loss of pixel data.
     * @return this canvas after a rotation by 270° clockwise, or 90° counterclockwise.
     */
   def rotate270: T
}
