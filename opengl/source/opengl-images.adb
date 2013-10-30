with
     GID,
     ada.Calendar;


package body openGL.Images
is


   function fetch_Image (Stream  : in Ada.Streams.Stream_IO.Stream_Access;
                         try_TGA : in Boolean) return opengl.Image
   is
      the_gid_Image : GID.Image_descriptor;
      next_Frame    : Ada.Calendar.Day_Duration := 0.0;

   begin
      GID.Load_image_header (the_gid_Image,
                             Stream.all,
                             try_tga => try_TGA);
      declare
         image_width  : constant Positive := GID.Pixel_Width  (the_gid_Image);
         image_height : constant Positive := GID.Pixel_height (the_gid_Image);

         the_Image : opengl.Image (1 .. Index_t (image_Height),
                                   1 .. Index_t (image_Width));
         --  Load the image
         --
         procedure Load_raw_image
         is
            subtype Primary_color_range is GL.glUByte;

            Row,
            Col : Index_t;

            procedure Set_X_Y (x, y : Natural) is
            begin
               Col := Index_t (X + 1);
               Row := Index_t (Y + 1);
            end Set_X_Y;

            procedure Put_Pixel (red, green, blue : Primary_color_range;
                                 alpha            : Primary_color_range)
            is
               use type GL.glUByte, Real;

               pragma Warnings (off, alpha);         -- alpha is just ignored
            begin
               the_Image (Row, Col) := (red, green, blue);

               if Col = Index_t (Image_Width) then   -- GID requires us to look to next pixel
                  Row := Row + 1;                    -- on the right for next time.
                  Col := 1;
               else
                  Col := Col + 1;
               end if;
            end Put_Pixel;

            procedure Feedback (percents : Natural) is null;

            procedure Load_image is
              new GID.Load_image_contents (Primary_color_range,
                                           set_X_Y,  put_Pixel,
                                           Feedback, GID.fast);
         begin
            load_Image (the_gid_Image, next_Frame);
         end load_raw_Image;

      begin
         load_raw_Image;
         return the_Image;
      end;
   end fetch_Image;


end openGL.Images;

