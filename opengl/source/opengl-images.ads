with
     Ada.Streams.Stream_IO;

package openGL.Images
--
--  Provides subprograms to create and manipulate images.
--
is

   function fetch_Image (Stream  : in Ada.Streams.Stream_IO.Stream_Access;
                         try_TGA : in Boolean) return opengl.Image;

end openGL.Images;
