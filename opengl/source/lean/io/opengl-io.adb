with
     GID,

     openGL.Images,
     openGL.Viewport,

     GL.lean,
     GL.Pointers,

     Ada.Unchecked_Conversion,
     Ada.Calendar,
     Ada.Characters.Handling;


package body openGL.IO
is
   use GL.lean,
       Ada.Characters.Handling,
       Ada.Streams.Stream_IO;

   use type Index_t;


   --  Face
   --

   function Vertices_of (Self : in Face) return Vertices
   is
   begin
      case Self.Kind
      is
         when Triangle =>   return Self.Tri;
         when Quad     =>   return Self.Quad;
         when Polygon  =>   return Self.Poly.all;
      end case;
   end Vertices_of;



   procedure set_Vertex_in (Self : in out Face;   Which : in Index_t;
                                                  To    : in Vertex)
   is
   begin
      case Self.Kind
      is
         when Triangle =>   Self.Tri  (Which) := To;
         when Quad     =>   Self.Quad (Which) := To;
         when Polygon  =>   Self.Poly (Which) := To;
      end case;
   end set_Vertex_in;



   procedure destroy (Self : in out Face)
   is
      procedure free is new Ada.Unchecked_Deallocation (Vertices, Vertices_view);
   begin
      if Self.Kind = Polygon then
         free (Self.Poly);
      end if;
   end destroy;




   --  general operations
   --

   function current_Frame return openGL.Image
   is
      use GL,
          GL.Pointers,
          openGL.Texture;

      the_Extent : constant openGL.Extent_2d := openGL.Viewport.Extent;
      the_Frame  :          openGL.Image (1 .. Index_t (the_Extent.Width),
                                          1 .. Index_t (the_Extent.Height));
   begin
      glReadPixels (0,
                    0,
                    GLsizei (the_Extent.Width),
                    GLsizei (the_Extent.Height),
                    to_GL (TexFormatEnm'(openGL.Texture.RGB)),
                    GL_UNSIGNED_BYTE,
                    to_GLvoid_access (the_Frame (1, 1).Red'Access));
      return the_Frame;
   end current_Frame;



   --  Forge
   --

   function to_height_Map (image_Filename : in String;
                           Scale          : in Real  := 1.0) return height_Map_view
   is
      f       :          Ada.Streams.Stream_IO.File_Type;
      image   :          GID.Image_descriptor;
      up_name : constant String                         := To_Upper (image_Filename);

      next_frame : Ada.Calendar.Day_Duration := 0.0;
   begin
      Open (f, In_File, image_Filename);

      GID.Load_image_header (image,
                             Stream (f).all,
                             try_tga =>          image_Filename'Length >= 4
                                        and then up_name (up_name'Last - 3 .. up_name'Last) = ".TGA");
      declare
         image_width  : constant        Positive   := GID.Pixel_width  (image);
         image_height : constant        Positive   := GID.Pixel_height (image);

         the_Heights  : constant access height_Map := new height_Map' (1 .. Index_t (image_height) =>
                                                                         (1 .. Index_t (image_width) => <>));
         --  Load the image.
         --
         procedure Load_raw_image
         is
            subtype Primary_color_range is GL.GLubyte;

            Row, Col : Index_t;

            procedure Set_X_Y (x, y : Natural)
            is
            begin
               Col := Index_t (x + 1);
               Row := Index_t (y + 1);
            end Set_X_Y;

            procedure Put_Pixel (red, green, blue : Primary_color_range;
                                 alpha            : Primary_color_range)
            is
               pragma Warnings (Off, alpha); -- alpha is just ignored
               use type GL.GLubyte, Real;
            begin
               the_Heights (Row, Col) :=   (Real (red) + Real (green) + Real (blue))
                                         / (3.0 * 255.0)
                                         * Scale;

               if Col = Index_t (image_width)
               then
                  Row := Row + 1;
                  Col := 1;
               else
                  Col := Col + 1;
               end if;

               --  ^ GID requires us to look to next pixel on the right for next time.
            end Put_Pixel;

            procedure Feedback (percents : Natural) is null;

            procedure Load_image is new GID.Load_image_contents (Primary_color_range,
                                                                 Set_X_Y,
                                                                 Put_Pixel,
                                                                 Feedback,
                                                                 GID.fast);
         begin
            Load_image (image, next_frame);
         end Load_raw_image;

      begin
         Load_raw_image;
         Close (f);

         return the_Heights.all'Unchecked_Access;
      end;
   end to_height_Map;



   function fetch_Image (Stream  : in Ada.Streams.Stream_IO.Stream_Access;
                         try_TGA : in Boolean) return openGL.Image
   is
   begin
      return openGL.Images.fetch_Image (Stream, try_TGA);
   end fetch_Image;



   function to_Image (image_Filename : in String) return openGL.Image
   is
      the_File :          Ada.Streams.Stream_IO.File_Type;
      up_Name  : constant String                         := To_Upper (image_Filename);
   begin
      open (the_File, In_File, image_Filename);

      declare
         the_Image : constant openGL.Image
           := fetch_Image (Stream (the_File),
                           try_TGA =>          image_Filename'Length >= 4
                                      and then up_Name (up_Name'Last - 3 .. up_Name'Last) = ".TGA");
      begin
         Close (the_File);
         return the_Image;
      end;
   end to_Image;



   function to_lucid_Image (image_Filename : in String) return openGL.lucid_Image
   is
      unused : aliased Boolean;
   begin
      return to_lucid_Image (image_Filename, unused'Access);
   end to_lucid_Image;



   function to_lucid_Image (image_Filename : in     String;
                            is_Lucid       : access Boolean) return openGL.lucid_Image
   is
      the_File   :          Ada.Streams.Stream_IO.File_Type;
      the_Image  :          GID.Image_descriptor;
      up_Name    : constant String                    := To_Upper (image_Filename);

      next_Frame :          Ada.Calendar.Day_Duration := 0.0;
   begin
      Open (the_File, In_File, image_Filename);

      GID.Load_image_header (the_Image,
                             Stream (the_File).all,
                             try_tga =>          image_Filename'Length >= 4
                                        and then up_Name (up_Name'Last - 3 .. up_Name'Last) = ".TGA");
      declare
         image_width  : constant Positive := GID.Pixel_width (the_Image);
         image_height : constant Positive := GID.Pixel_height (the_Image);

         the_Frame    : openGL.lucid_Image (1 .. Index_t (image_height),
                                            1 .. Index_t (image_width));
         --  Load the image
         --
         procedure Load_raw_image
         is
            subtype Primary_color_range is GL.GLubyte;

            Row, Col : Index_t;

            procedure Set_X_Y (x, y : Natural)
            is
            begin
               Col := Index_t (x + 1);
               Row := Index_t (y + 1);
            end Set_X_Y;

            procedure Put_Pixel (red, green, blue : Primary_color_range;
                                 alpha            : Primary_color_range)
            is
               use type GL.GLubyte, Real;
            begin
               the_Frame (Row, Col) := ((red, green, blue), alpha);

               if Col = Index_t (image_width)
               then                             -- GID requires us to look to next pixel on the right for next time
                  Row := Row + 1;
                  Col := 1;
               else
                  Col := Col + 1;
               end if;

               if alpha /= Opaque then
                  is_Lucid.all := True;
               end if;
            end Put_Pixel;

            procedure Feedback (percents : Natural) is null;

            procedure Load_image is new GID.Load_image_contents (Primary_color_range,
                                                                 Set_X_Y,
                                                                 Put_Pixel,
                                                                 Feedback,
                                                                 GID.fast);
         begin
            Load_image (the_Image, next_Frame);
         end Load_raw_image;

      begin
         is_Lucid.all := False;

         Load_raw_image;
         Close (the_File);

         return the_Frame;
      end;
   end to_lucid_Image;



   function to_Texture (image_Filename : in String) return openGL.Texture.Object
   is
      use openGL.Texture;

      is_Lucid        : aliased  Boolean;
      the_lucid_Image : constant openGL.lucid_Image := to_lucid_Image (image_Filename, is_Lucid'Access);
      the_Texture     :          Texture.Object     := to_Texture (the_lucid_Image'Length (2), the_lucid_Image'Length (1));
   begin
      if is_Lucid
      then
         set_Image (the_Texture, the_lucid_Image);
      else
         declare
            the_opaque_Image : constant openGL.Image := to_Image (the_lucid_Image);
         begin
            set_Image (the_Texture, the_opaque_Image);
         end;
      end if;

      return the_Texture;
   end to_Texture;



   procedure destroy (Self : in out Model)
   is
      procedure free is new Ada.Unchecked_Deallocation (openGL.Sites,                 Sites_view);
      procedure free is new Ada.Unchecked_Deallocation (openGL.Coordinates_2D,        Coords_view);
      procedure free is new Ada.Unchecked_Deallocation (openGL.Normals,               Normals_view);
      procedure free is new Ada.Unchecked_Deallocation (bone_Weights,                 bone_Weights_view);
      procedure free is new Ada.Unchecked_Deallocation (openGL.IO.bone_weights_Array, bone_weights_Array_view);
      procedure free is new Ada.Unchecked_Deallocation (openGL.IO.Faces,              Faces_view);
   begin
      free (Self.Sites);
      free (Self.Coords);
      free (Self.Normals);

      if Self.Weights /= null
      then
         for Each in Self.Weights'Range
         loop
            free (Self.Weights (Each));
         end loop;

         free (Self.Weights);
      end if;

      for Each in Self.Faces'Range
      loop
         destroy (Self.Faces (Each));
      end loop;

      free (Self.Faces);
   end destroy;




   --- Screenshot and video capture
   --

   type U8  is mod 2 **  8;   for U8 'Size use  8;
   type U16 is mod 2 ** 16;   for U16'Size use 16;
   type U32 is mod 2 ** 32;   for U32'Size use 32;

   --  type I16 is range -2 ** 15 .. 2 ** 15 - 1; for I16'Size use 16;
   type I32 is range -2 ** 31 .. 2 ** 31 - 1;   for I32'Size use 32;



   generic
      type Number is mod <>;
      s : Stream_Access;
   procedure Write_Intel_x86_number (n : in Number);



   procedure Write_Intel_x86_number (n : in Number)
   is
      m     : Number           := n;
      bytes : constant Integer := Number'Size / 8;
   begin
      for i in 1 .. bytes
      loop
         U8'Write (s, U8 (m mod 256));
         m := m / 256;
      end loop;
   end Write_Intel_x86_number;



   procedure Write_raw_BGR_frame (s             : Stream_Access;
                                  width, height : Natural)
   is
      use GL, GL.Pointers, openGL.Texture;

      type Temp_bitmap_type is array (Natural range <>) of aliased GLubyte;

      PicData : Temp_bitmap_type (0 .. (width + 4) * (height + 4) * 3 - 1);

      --  No dynamic allocation needed!
      --  The "+4" are there to avoid parity address problems when GL writes to the buffer.

      --        type loc_pointer is new gl.GLvoid_Pointer;

      --       function Cvt is new Ada.Unchecked_Conversion(System.Address,loc_pointer);
      --       -- This method is functionally identical as GNAT's Unrestricted_Access
      --       -- but has no type safety (cf GNAT Docs)

      --       pragma No_Strict_Aliasing(loc_pointer); -- recommended by GNAT 2005+
      --        pPicData : loc_pointer;
   begin
      --        pPicData := Cvt(PicData(0)'Address);

      glReadPixels (0,
                    0,
                    GLsizei (width),
                    GLsizei (height),
                    to_GL (openGL.Texture.BGR),
                    GL_UNSIGNED_BYTE,
                    to_GLvoid_access (PicData (0)'Access));

      Temp_bitmap_type'Write (s,
                              PicData (0 .. width * height * 3 - 1));
   end Write_raw_BGR_frame;



   procedure Screenshot (Filename : in String)
   is
      use GL;

      --  Translated by (New) P2Ada v. 15-Nov-2006 http://wiki.delphigl.com/index.php/Screenshot
      f : Ada.Streams.Stream_IO.File_Type;

      type BITMAPFILEHEADER is
         record
            bfType      : U16;
            bfSize      : U32;
            bfReserved1 : U16;
            bfReserved2 : U16;
            bfOffBits   : U32;
         end record;
      pragma Pack (BITMAPFILEHEADER);
      for BITMAPFILEHEADER'Size use 8 * 14;


      type BITMAPINFOHEADER is
         record
            biSize          : U32;
            biWidth         : I32;
            biHeight        : I32;
            biPlanes        : U16;
            biBitCount      : U16;
            biCompression   : U32;
            biSizeImage     : U32;
            biXPelsPerMeter : I32;
            biYPelsPerMeter : I32;
            biClrUsed       : U32;
            biClrImportant  : U32;
         end record;
      pragma Pack (BITMAPINFOHEADER);
      for BITMAPINFOHEADER'Size use 8 * 40;


      FileInfo   : BITMAPINFOHEADER;
      FileHeader : BITMAPFILEHEADER;
      --        type intPtr is new intPointer;
      Viewport : array (0 .. 3) of aliased GLint;

      --        function Cvt is new Ada.Unchecked_Conversion(System.Address,intPtr);

      --  This method is functionally identical as GNAT's Unrestricted_Access
      --  but has no type safety (cf GNAT Docs)
      --        pragma No_Strict_Aliasing(intPtr); -- recommended by GNAT 2005+
   begin
      glGetIntegerv (GL_VIEWPORT,
                     Viewport (0)'Unchecked_Access);

      FileHeader.bfType        := 16#4D42#; -- 'BM'
      FileHeader.bfOffBits     :=   BITMAPINFOHEADER'Size / 8
                                  + BITMAPFILEHEADER'Size / 8;
      FileHeader.bfReserved1   := 0;
      FileHeader.bfReserved2   := 0;

      FileInfo.biSize          := BITMAPINFOHEADER'Size / 8;
      FileInfo.biWidth         := I32 (Viewport (2));
      FileInfo.biHeight        := I32 (Viewport (3));
      FileInfo.biPlanes        := 1;
      FileInfo.biBitCount      := 24;
      FileInfo.biCompression   := 0;
      FileInfo.biSizeImage     :=   U32 (FileInfo.biWidth * FileInfo.biHeight)
                                  * U32 (FileInfo.biBitCount / 8);
      FileInfo.biXPelsPerMeter := 0;
      FileInfo.biYPelsPerMeter := 0;
      FileInfo.biClrUsed       := 0;
      FileInfo.biClrImportant  := 0;

      FileHeader.bfSize        := FileHeader.bfOffBits + FileInfo.biSizeImage;

      Create (f, Out_File, Filename);
      declare
         procedure Write_Intel is new Write_Intel_x86_number (U16, Stream (f));
         procedure Write_Intel is new Write_Intel_x86_number (U32, Stream (f));
         function  Cvt         is new Ada.Unchecked_Conversion (I32, U32);
      begin
         -- ** Only for Intel endianess: ** --
         --  BITMAPFILEHEADER'Write(Stream(F), FileHeader);
         --  BITMAPINFOHEADER'Write(Stream(F), FileInfo);
         --
         -- ** Endian-safe: ** --
         Write_Intel (FileHeader.bfType);
         Write_Intel (FileHeader.bfSize);
         Write_Intel (FileHeader.bfReserved1);
         Write_Intel (FileHeader.bfReserved2);
         Write_Intel (FileHeader.bfOffBits);
         --
         Write_Intel (FileInfo.biSize);
         Write_Intel (Cvt (FileInfo.biWidth));
         Write_Intel (Cvt (FileInfo.biHeight));
         Write_Intel (FileInfo.biPlanes);
         Write_Intel (FileInfo.biBitCount);
         Write_Intel (FileInfo.biCompression);
         Write_Intel (FileInfo.biSizeImage);
         Write_Intel (Cvt (FileInfo.biXPelsPerMeter));
         Write_Intel (Cvt (FileInfo.biYPelsPerMeter));
         Write_Intel (FileInfo.biClrUsed);
         Write_Intel (FileInfo.biClrImportant);
         --
         Write_raw_BGR_frame (Stream (f),
                              Integer (Viewport (2)),
                              Integer (Viewport (3)));
         Close (f);

      exception
         when others =>
            Close (f);
            raise;
      end;
   end Screenshot;



   -------------------
   -- Video capture --
   -------------------

   --  Exceptionally we define global variables since it is not expected
   --  that more that one capture is taken at the same time.
   --
   avi           : Ada.Streams.Stream_IO.File_Type;
   frames        : Natural;
   rate          : Positive;
   width, height : Positive;
   bmp_size      : U32;

   procedure Write_RIFF_headers
   is
      --  Written 1st time to take place (but # of frames unknown)
      --  Written 2nd time for setting # of frames, sizes, etc.
      --
      calc_bmp_size    : constant U32           := U32 (((width)) * height * 3);
      --  !! stuff to multiple of 4 !!
      index_size       : constant U32           := U32 (frames) * 16;
      movie_size       : constant U32           := 4 + U32 (frames) * (calc_bmp_size + 8);
      second_list_size : constant U32           := 4 + 64 + 48;
      first_list_size  : constant U32           := (4 + 64) + (8 + second_list_size);
      file_size        : constant U32           := 8 + (8 + first_list_size) + (4 + movie_size) + (8 + index_size);
      s                : constant Stream_Access := Stream (avi);

      procedure Write_Intel is new Write_Intel_x86_number (U16, s);
      procedure Write_Intel is new Write_Intel_x86_number (U32, s);

      microseconds_per_frame : constant U32     := U32 (1_000_000.0 / Long_Float (rate));
   begin
      bmp_size := calc_bmp_size;

      String'Write (s, "RIFF");
      U32'Write (s, file_size);
      String'Write (s, "AVI ");
      String'Write (s, "LIST");
      Write_Intel (first_list_size);
      String'Write (s, "hdrl");
      String'Write (s, "avih");
      Write_Intel (U32'(56));

      --  Begin of AVI Header
      Write_Intel (microseconds_per_frame);
      Write_Intel (U32'(0));      -- MaxBytesPerSec
      Write_Intel (U32'(0));      -- Reserved1
      Write_Intel (U32'(16));     -- Flags (16 = has an index)
      Write_Intel (U32 (frames));
      Write_Intel (U32'(0));      -- InitialFrames
      Write_Intel (U32'(1));      -- Streams
      Write_Intel (bmp_size);
      Write_Intel (U32 (width));
      Write_Intel (U32 (height));
      Write_Intel (U32'(0));      -- Scale
      Write_Intel (U32'(0));      -- Rate
      Write_Intel (U32'(0));      -- Start
      Write_Intel (U32'(0));      -- Length
      --  End of AVI Header

      String'Write (s, "LIST");
      Write_Intel (second_list_size);
      String'Write (s, "strl");

      --  Begin of Str
      String'Write (s, "strh");
      Write_Intel (U32'(56));
      String'Write (s, "vids");
      String'Write (s, "DIB ");
      Write_Intel (U32'(0));                -- flags
      Write_Intel (U32'(0));                -- priority
      Write_Intel (U32'(0));                -- initial frames
      Write_Intel (microseconds_per_frame); -- Scale
      Write_Intel (U32'(1_000_000));        -- Rate
      Write_Intel (U32'(0));                -- Start
      Write_Intel (U32 (frames));           -- Length
      Write_Intel (bmp_size);               -- SuggestedBufferSize
      Write_Intel (U32'(0));                -- Quality
      Write_Intel (U32'(0));                -- SampleSize
      Write_Intel (U32'(0));
      Write_Intel (U16 (width));
      Write_Intel (U16 (height));
      --  End of Str

      String'Write (s, "strf");
      Write_Intel (U32'(40));

      --  Begin of BMI
      Write_Intel (U32'(40));    -- BM header size (like BMP)
      Write_Intel (U32 (width));
      Write_Intel (U32 (height));
      Write_Intel (U16'(1));     -- Planes
      Write_Intel (U16'(24));    -- BitCount
      Write_Intel (U32'(0));     -- Compression
      Write_Intel (bmp_size);    -- SizeImage
      Write_Intel (U32'(3780));  -- XPelsPerMeter
      Write_Intel (U32'(3780));  -- YPelsPerMeter
      Write_Intel (U32'(0));     -- ClrUsed
      Write_Intel (U32'(0));     -- ClrImportant
      --  End of BMI

      String'Write (s, "LIST");
      Write_Intel (movie_size);
      String'Write (s, "movi");
   end Write_RIFF_headers;



   procedure Start_capture (AVI_name   : String;
                            frame_rate : Positive)
   is
      use GL;

      --        type intPtr is new intPointer;
      Viewport : array (0 .. 3) of aliased GLint;
      --        function Cvt is new Ada.Unchecked_Conversion(System.Address,intPtr);
      --  This method is functionally identical as GNAT's Unrestricted_Access
      --  but has no type safety (cf GNAT Docs)
      --        pragma No_Strict_Aliasing(intPtr); -- recommended by GNAT 2005+
   begin
      Create (avi, Out_File, AVI_name);

      frames := 0;
      rate   := frame_rate;

      glGetIntegerv (GL_VIEWPORT,
                     Viewport (0)'Unchecked_Access);

      width  := Positive (Viewport (2));
      height := Positive (Viewport (3));
      --  NB: GL viewport resizing should be blocked during the video capture!
      Write_RIFF_headers;
   end Start_capture;



   procedure Capture_frame
   is
      s : constant Stream_Access := Stream (avi);
      procedure Write_Intel is new Write_Intel_x86_number (U32, s);
   begin
      String'Write        (s, "00db");
      Write_Intel         (bmp_size);
      Write_raw_BGR_frame (s, width, height);

      frames := frames + 1;
   end Capture_frame;



   procedure Stop_capture
   is
      index_size  : constant U32           := U32 (frames) * 16;
      s           : constant Stream_Access := Stream (avi);
      ChunkOffset :          U32           := 4;
      procedure Write_Intel is new Write_Intel_x86_number (U32, s);
   begin
      --  write the index section
      String'Write (s, "idx1");
      Write_Intel (index_size);

      for f in 1 .. frames
      loop
         String'Write (s, "00db");
         Write_Intel  (U32'(16));   -- keyframe
         Write_Intel  (ChunkOffset);
         ChunkOffset := ChunkOffset + bmp_size + 8;
         Write_Intel  (bmp_size);
      end loop;

      --  Go back to file beginning...
      Set_Index (avi, 1);
      Write_RIFF_headers;   -- rewrite headers with correct data
      Close (avi);
   end Stop_capture;


end openGL.IO;
