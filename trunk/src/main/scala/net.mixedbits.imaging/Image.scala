package net.mixedbits.imaging

import java.io._
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Rectangle
import java.awt.Point
import java.awt.RenderingHints
import java.awt.Color
import java.awt.color._
import java.awt.image._
import java.awt.geom.AffineTransform
import java.util._
import javax.media.jai._
import javax.media.jai.operator._


/**
* This class is intended to act as a simplified and reliable wrapper to jai.
* It is the preferred class to use when performing image operations.
*/
class Image private(val image:RenderedImage){
  
  
  /**
  * Creates a new Image by copying the specified RenderedImageAdapter
  */
  protected def this(adapter:RenderedImageAdapter) = 
    this(new RenderedImageAdapter(adapter.getAsBufferedImage).asInstanceOf[RenderedImage])
  
  /**
  * Constructs a new transparent Image with the specified dimensions
  */
  def this(width:Int, height:Int) =
    this(new RenderedImageAdapter(new BufferedImage(width,height,BufferedImage.TYPE_INT_ARGB)))
  
  /**
  * Constructs a new Image with the specified dimensions, using the specified color as the background color
  */
  def this(width:Int, height:Int, fillColor:Color) = 
    this({
      val baseImage = new BufferedImage(width,height,BufferedImage.TYPE_INT_ARGB)
      val graphics = baseImage.createGraphics()
      graphics.setColor(fillColor)
      graphics.fillRect(0,0,width,height)
      graphics.dispose()
      new RenderedImageAdapter(baseImage)
      })
  
  /**
  * Loads the Image from the specified file
  */
  def this(file:File) = 
    this(JAI.create("fileload", file.getAbsolutePath()))
  
  /**
  * Loads the Image from the specified InputStream
  */
  def this(input:InputStream) = 
    this(JAI.create("stream", new com.sun.media.jai.codec.ForwardSeekableStream(input)))
  
  /**
  * Creates a new Image by copying the specified Image
  */
  def this(source:Image) = 
    this(new RenderedImageAdapter(new RenderedImageAdapter(source.image).getAsBufferedImage)) //Does this copy the image, is this being excessive?
  
  /**
  * Creates a new image from the specified BufferedImage
  */
  def this(image:BufferedImage) = 
    this(new RenderedImageAdapter(image))


  def width() = image.getWidth    
  def height() = image.getHeight

  def dimension() = new Dimension(this.width,this.height)
  
  def resolution() = (width,height)


  def scaledToWidth(piWidth:Int):Image = {
    val lfScaleFactor = piWidth.toFloat / this.width.toFloat
    val liHeight = (lfScaleFactor * this.height).toInt
    scaledImage(piWidth,liHeight)
  }
  
  def scaledToHeight(piHeight:Int):Image = {
    val lfScaleFactor = piHeight.toFloat / this.height.toFloat
    val liWidth = (lfScaleFactor * this.width).toInt
    scaledImage(liWidth,piHeight)
  }

  def scaledImage(piWidth:Int, piHeight:Int):Image = {
    
    //These settings will prevent a black border from surrounding the image when operations did not result in complete pixels
    val borderHints = new RenderingHints(JAI.KEY_BORDER_EXTENDER, BorderExtender.createInstance(BorderExtender.BORDER_COPY))
    
    
    //Blur based on scale factor, this will reduce the noise, making the resulting image appear smoother
    //	this scale factor causes the blur to be higher the smaller the image is being scaled
    val lfScaleX = (piWidth.toFloat / image.getWidth.toFloat)
    val lfScaleY = (piHeight.toFloat / image.getHeight.toFloat)
    val lfBlurFactor = (image.getWidth.toFloat / piWidth.toFloat) * 0.60f

    val preparedImage =
      if(lfBlurFactor > 0.60f) // NOTE: This check causes all upwards scaling to be non bluring
        ConvolveDescriptor.create(image,createGaussianKernel(lfBlurFactor),borderHints)
      else
        image

    
    //Scale
    val scaledImage = ScaleDescriptor.create(preparedImage,lfScaleX,lfScaleY,0.0f,0.0f,Interpolation.getInstance(Interpolation.INTERP_BICUBIC),borderHints)

    new Image(scaledImage)
  }

  def scaledImage(piWidth:Int, piHeight:Int, poMode:ScaleMode):Image = 
    scaledImage(piWidth,piHeight,poMode,ImageAlign.TopLeft)

  def scaledImage(piWidth:Int, piHeight:Int, poMode:ScaleMode, poAlignment:ImageAlign):Image = {
    //Handle stretch first
    if(poMode == ScaleMode.Stretch)
      return scaledImage(piWidth,piHeight);

    val lfRatioX						= piWidth.toFloat / this.width.toFloat
    val lfRatioY						= piHeight / this.height.toFloat

    //Handle Fill and Show (Aspect preserving modes)
    if(poMode == ScaleMode.Show || poMode == ScaleMode.Fit){
      //Scale image so entire photo shows
      val scaledImage = 
        if(lfRatioX < lfRatioY)
          this.scaledToWidth(piWidth)
        else
          this.scaledToHeight(piHeight)
    
      //Don't pad image to fit desired width and height, just return it as is
      if(poMode == ScaleMode.Fit)
        return scaledImage

      val position = poAlignment.overlayedPosition(new Dimension(piWidth,piHeight),scaledImage.dimension())

      //Position image
      return (new Image(piWidth,piHeight)).overlayImage(scaledImage,position.x,position.y)
    }
    else if(poMode == ScaleMode.Fill){
      //Scale image to fill entire area
    	val scaledImage = 
        if(lfRatioX > lfRatioY)
        	this.scaledToWidth(piWidth)
        else
        	this.scaledToHeight(piHeight)
        
      //Position and crop
      return scaledImage.croppedImage(piWidth,piHeight,poAlignment);
    }

    throw new IllegalArgumentException("No valid mode specified!");
  }

  def croppedImage(offsetX:Int, offsetY:Int, width:Int, height:Int):Image = 
  	new Image(CropDescriptor.create(image,offsetX.toFloat,offsetY.toFloat,width.toFloat,height.toFloat,null))
  
  def croppedImage(rectangle:Rectangle):Image = 
  	new Image(CropDescriptor.create(image,rectangle.x.toFloat,rectangle.y.toFloat,rectangle.width.toFloat,rectangle.height.toFloat,null))
  
  def croppedImage(newWidth:Int, newHeight:Int, alignment:ImageAlign):Image = {
    
    val position = alignment.overlayedPosition(new Dimension(newWidth,newHeight),this.dimension);
    
    val (drawX, cropX, cropWidth) = 
      if(position.x < 0){
        val cropMax					= this.width()+position.x; 
        (0,-position.x,Math.min(cropMax,newWidth))
        }
      else{
        val displayMax				= newWidth-position.x;
        (position.x,0,Math.min(displayMax,this.width()))
        }
    
    val (drawY, cropY, cropHeight) = 
      if(position.y < 0){
        val cropMax					= this.height()+position.y;
        (0,-position.y,Math.min(cropMax,newHeight))
        }
      else{
        val displayMax				= newHeight-position.y;
        (position.y,0,Math.min(displayMax,this.height()))
        }
    
    if(cropWidth <= 0 || cropHeight <= 0)
    	return this
    
    return croppedImage(cropX,cropY,cropWidth,cropHeight)
  }

  
  
  def overlayImage(overlay:Image):Image = 
    overlayImage(overlay, 0, 0)

  def overlayImage(overlay:Image, alignment:ImageAlign):Image = {
    val position = alignment.overlayedPosition(this.dimension,overlay.dimension)
    overlayImage(overlay, position.x, position.y)
    }

  def overlayImage(overlay:Image, offsetX:Int, offsetY:Int):Image = {
    //Use awt based overlay, its safer
    
    val bufferedImage = new RenderedImageAdapter(image).getAsBufferedImage
    val bufferedOverlay = new RenderedImageAdapter(overlay.image).getAsBufferedImage

    val (drawX,cropX,cropWidth) = 
      if(offsetX < 0){
        val cropMax = bufferedOverlay.getWidth()+offsetX;
        (0,-offsetX,Math.min(cropMax,bufferedImage.getWidth()))
      }
      else{
        val displayMax = bufferedImage.getWidth()-offsetX;
        (offsetX,0,Math.min(displayMax,bufferedOverlay.getWidth()))
      }
    
    val (drawY,cropY,cropHeight) = 
      if(offsetY < 0){ 
        val cropMax = bufferedOverlay.getHeight()+offsetY
        (0,-offsetY,Math.min(cropMax,bufferedImage.getHeight()))
      }
      else{
        val displayMax = bufferedImage.getHeight()-offsetY
        (offsetY,0,Math.min(displayMax,bufferedOverlay.getHeight()))
      }
    
    if(cropWidth <= 0 || cropHeight <= 0)
      return new Image(bufferedImage)
    
    val graphics = bufferedImage.createGraphics()
    graphics.drawImage(bufferedOverlay.getSubimage(cropX,cropY,cropWidth,cropHeight),drawX,drawY,null)
    graphics.dispose()
    
    return new Image(bufferedImage)
  }
  
  def toRGB():Image = 
    if(image.getSampleModel.getNumBands == 4)
      new Image(BandSelectDescriptor.create(image, Array(0,1,2) ,null))
    else
      this
    
  private val defaultJpegQuality = 0.75f;
  
  def saveJpeg(destination:File){ saveJpeg(destination,defaultJpegQuality) }
    
  def saveJpeg(destination:File, quality:Int){ saveJpeg(destination,(quality/100.0f)) }

  def saveJpeg(destination:File, quality:Float){
    //Convert ARGB to RGB before saving as jpeg
    saveAs(toRGB().image,destination,"jpeg",jpegEncoding(quality));
  }
  
  private def jpegEncoding(quality:Int):com.sun.media.jai.codec.JPEGEncodeParam = 
    jpegEncoding(quality/100.0f)
  
  private def jpegEncoding(quality:Float):com.sun.media.jai.codec.JPEGEncodeParam = {
    val encoding = new com.sun.media.jai.codec.JPEGEncodeParam()
    encoding.setQuality(quality)
    encoding    
  }
    

  def savePng(destination:File){ saveAs(image,destination,"png",null) }
  
  protected def saveAs(image:RenderedImage, destination:File, format:String, encoding:com.sun.media.jai.codec.ImageEncodeParam){
    //Delete before writing, this will keep big files being overwritten by smaller files from leaving garbage, which would result in a file the original size
    if(destination.exists)
      destination.delete()
    
    FileStoreDescriptor.create(image,destination.getAbsolutePath,format,encoding,true,null);
  }
  
  def writePng(outputStream:OutputStream) = writeImage(image,outputStream,"png",null)
  def writeJpeg(outputStream:OutputStream){ writeJpeg(outputStream,defaultJpegQuality) }
  def writeJpeg(outputStream:OutputStream,quality:Int){ writeJpeg(outputStream,(quality/100.0f)) }
  def writeJpeg(outputStream:OutputStream,quality:Float){ writeImage(toRGB.image,outputStream,"jpeg",jpegEncoding(quality)) }
  
  def writeImage(image:RenderedImage, outputStream:OutputStream,format:String, encoding:com.sun.media.jai.codec.ImageEncodeParam){
    com.sun.media.jai.codec.ImageCodec.createImageEncoder(format,outputStream,encoding).encode(image)
  }
  
  private def createGaussianKernel(radius:Float):KernelJAI = {
    val diameter:Int = ((2*radius) + 1).toInt
    val invrsq:Float = 1.0f/(radius*radius)
    
    val gaussianData = new Array[Float](diameter)
    
    var sum = 0.0f
    for(i <- 0 until diameter){
      val d:Float = i - radius
      val value:Float = Math.exp(-d*d*invrsq).toFloat
      gaussianData(i) = value
      sum += value
    }
    
    // Normalize
    val invsum:Float = 1.0f/sum
    for(i <- 0 until diameter){
      gaussianData(i) *= invsum
    }
    
    new KernelJAI(diameter, diameter, radius.toInt, radius.toInt, gaussianData, gaussianData)
  }
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class ImageAlign(alignment:Int){

  private def anchorTop = (alignment & ImageAlign.Top) == ImageAlign.Top
  private def anchorBottom = (alignment & ImageAlign.Bottom) == ImageAlign.Bottom
  private def anchorLeft = (alignment & ImageAlign.Left) == ImageAlign.Left
  private def anchorRight = (alignment & ImageAlign.Right) == ImageAlign.Right
  
  /**
  * @param base			the dimensions of the image which the overlay will be placed relative to
  * @param overlay		the dimensions of the image which will be overlayed on the base image
  * @return				a point specifying the location of the overlay image relative to the base image
  */
  def overlayedPosition(base:Dimension, overlay:Dimension):Point = {
    
    val offsetX = 
      if(anchorLeft && anchorRight)
        (base.width - overlay.width) / 2 //Center horizontally
      else if(anchorRight)
        (base.width - overlay.width) //Align right
      else
        0
      
    val offsetY = 
      if(anchorTop && anchorBottom)
        (base.height - overlay.height) / 2 //Center vertically
      else if(anchorBottom)
        (base.height - overlay.height) //Align bottom
      else
        0
    
    new Point(offsetX, offsetY)
  }
}
object ImageAlign{
  private val Top	= 1
  private val Bottom = 2
  private val Left = 4
  private val Right	= 8
  
  case object TopLeft extends ImageAlign(Top|Left)
  case object CenterLeft extends ImageAlign(Top|Bottom|Left)
  case object BottomLeft extends ImageAlign(Bottom|Left)
  case object TopCenter extends ImageAlign(Top|Left|Right)
  case object CenterCenter extends ImageAlign(Top|Bottom|Left|Right)
  case object BottomCenter extends ImageAlign(Bottom|Left|Right)
  case object TopRight extends ImageAlign(Top|Right)
  case object CenterRight extends ImageAlign(Top|Bottom|Right)
  case object BottomRight extends ImageAlign(Bottom|Right)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class ScaleMode
object ScaleMode{
  /** Show mode will display the entire image while preserving aspect ratio, and leave transparent padding based on the ImageAlign settings used. */
  case object Show extends ScaleMode
  /** Fit mode will display the entire image while preserving aspect ratio,  by fitting it within the constraints of the width and height specified, but will only be as wide and as tall as the resulting image.  ImageAlign settings will not effect this mode. */
  case object Fit extends ScaleMode
  /** Fill mode will fill the specified display area while preserving aspect ratio, any area which extends over the bounds of the specified area will be cropped based on the ImageAlign settings. */
  case object Fill extends ScaleMode
  /** Stretch mode will fill the specified area by distorting the image to fit within the bounds of the area.  No data will be cropped, and aspect ratio will net be preserved. */
  case object Stretch extends ScaleMode
}
