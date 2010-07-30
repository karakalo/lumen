Title: Narrative Description of the Lumen.Image Packages

This is a simple narrative description of the packages in the `Lumen.Image`
family.

# Contents

* [Lumen.Image](#lumen-image)
* [Lumen.Image.BMP](#lumen-image-ppm)
* [Lumen.Image.PPM](#lumen-image-bmp)

------------------------------------------------------------------------------

# Lumen.Image {#lumen-image}

Right now this package has a couple of serious limitations:

* It can only read images, not write them.  So adding a "screenshot" feature
  to your app would be a manual procedure right now.

* It can read only binary PBM, PGM, and PPM image files, which are
  [the netpbm image formats][netpbm], and certain forms of BMP files.  While
  those formats are widely recognized, they're not sufficient, and more
  formats will be added "soon".  Near-term plans are for PNG and JPG, at
  least.  Probably won't ever add GIF, or PAM, or ASCII netpbm.  Things like
  TIFF and Targa and FITS and so on will be determined by demand.

But even with those limitations, it's still useful.  It accepts a pathname,
sniffs the file to determine what format it's in, and calls the appropriate
format-specific routine to read and decode it into Lumen's internal image
format, which is 8-bit RGBA.

That is, in Lumen images are a rectangular matrix of pixels, and each pixel is
4 bytes: one byte each for the red, green, and blue color values, and one byte
of "alpha", or the pixel's level of transparency.  All four values are
unsigned bytes, meaning they range from 0 to 255.  You can certainly also get
an image from other sources besides a file, like generating it yourself;
currently this package will only help you if you want to read it from a PPM or
BMP file.

There's also a `Complete` flag in the image descriptor as returned by the
`From_File` function.  If that flag is false, the image file was truncated or
otherwise corrupted and the library couldn't load the entire thing as
advertised.  In that case it will fill the unread part of the image values
with the "transparent" pixel.

------------------------------------------------------------------------------

# Lumen.Image.BMP {#lumen-image-bmp}

This is more or less an internal package that knows how to handle only BMP
images.  You'd normally use `Lumen.Image` instead of this one.  Apps *can*
call this package's services directly, though we can't imagine why they'd want
to.

------------------------------------------------------------------------------

# Lumen.Image.PPM {#lumen-image-ppm}

This is more or less an internal package that knows how to handle only PPM
images.  You'd normally use `Lumen.Image` instead of this one.  Apps *can*
call this package's services directly, though we can't imagine why they'd want
to.

------------------------------------------------------------------------------

[netpbm]: http://en.wikipedia.org/wiki/Netpbm_format
