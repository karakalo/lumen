with
     openGL.Errors,
     GL.lean,
     GL.Pointers,
     GLU;


package body openGL.Texture
is

   use GL,
       GL.lean,
       GL.Pointers,
       GLU;


   --  Texture object names.
   --

   function new_texture_Name return texture_Name
   is
      the_Name : aliased texture_Name;
   begin
      glGenTextures (1, the_Name'Unchecked_Access);
      return the_Name;
   end new_texture_Name;



   procedure free (the_texture_Name : in texture_Name)
   is
      the_Name : aliased texture_Name := the_texture_Name;
   begin
      glDeleteTextures (1, the_Name'Unchecked_Access);
   end free;



   --  Texture objects.
   --

   function to_Texture (Name : in texture_Name) return Object
   is
      Self : Texture.Object;
   begin
      Self.Name := Name;

      --  tbd: fill in remaining fields by querying gl

      return Self;
   end to_Texture;



   function to_Texture (min_Width, min_Height : in Positive) return Object
   is
      Self        : aliased Texture.Object;

      Size_width  : constant Size := to_Size (min_Width);
      Size_height : constant Size := to_Size (min_Height);

   begin
      Self.Size_width  := Size_width;
      Self.Size_height := Size_height;

      Self.Name := new_texture_Name;
      enable (Self);

      glPixelStorei (GL_UNPACK_ALIGNMENT, 1);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      glTexImage2D (GL.GL_TEXTURE_2D,
                    0,
                    GL.GL_RGB,
                    power_of_2_Ceiling (min_Width),
                    power_of_2_Ceiling (min_Height),
                    0,
                    GL.GL_RGB,
                    GL.GL_UNSIGNED_BYTE,
                    null);

      return Self;
   end to_Texture;



   function to_Texture (the_Image   : in openGL.Image;
                        use_Mipmaps : in Boolean     := True) return Object
   is
      Self : aliased Texture.Object;
   begin
      Self.Name := new_texture_Name;
      set_Image (Self, the_Image, use_Mipmaps);

      return Self;
   end to_Texture;



   function to_Texture (the_Image   : in openGL.lucid_Image;
                        use_Mipmaps : in Boolean           := True) return Object
   is
      Self : aliased Texture.Object;
   begin
      Self.Name := new_texture_Name;
      set_Image (Self, the_Image, use_Mipmaps);

      return Self;
   end to_Texture;



   procedure destroy (Self : in out Object)
   is
   begin
      --        if self.Pool = null then
      free (Self.Name);
      --        else
      --           free (self.Pool.all, Self);
      --        end if;
   end destroy;



   procedure set_Name (Self : in out Object;   To : in texture_Name)
   is
   begin
      Self.Name := To;
   end set_Name;



   function Name (Self : in Object) return texture_Name
   is
   begin
      return Self.Name;
   end Name;






   procedure set_Image (Self : in out Object;   To          : in openGL.Image;
                                                use_Mipmaps : in Boolean     := True)
   is
      use type Real;
      the_Image : openGL.Image renames To;
   begin
      declare
         min_Width   : constant Positive := the_Image'Length (2);
         min_Height  : constant Positive := the_Image'Length (1);

         Width       : constant GLsizei  := power_of_2_Ceiling (min_Width);
         Height      : constant GLsizei  := power_of_2_Ceiling (min_Height);

         Size_width  : constant Size     := to_Size (min_Width);
         Size_height : constant Size     := to_Size (min_Height);

      begin
         Self.is_Transparent := False;

         Self.Size_width  := Size_width;
         Self.Size_height := Size_height;

         enable (Self);

         glPixelStorei (GL_UNPACK_ALIGNMENT, 1);

         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
         glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

         openGL.Errors.log;  -- tbd: only for debug.

         if        min_Width  /= Positive (Width)
           or else min_Height /= Positive (Height)
         then
            declare
               scaled_Image : aliased openGL.Image (1 .. Index_t (Height),
                                                    1 .. Index_t (Width));
            begin
               gluScaleImage (GL_RGB,
                              GLint (min_Width),
                              GLint (min_Height),
                              GL_UNSIGNED_BYTE,
                              the_Image (1, 1).Red'Address,
                              Width,
                              Height,
                              GL_UNSIGNED_BYTE,
                              scaled_Image (1, 1).Red'Address);
               openGL.Errors.log;  -- tbd: only for debug.

               glTexImage2D (GL_TEXTURE_2D,
                             0,
                             GL_RGB,
                             Width,
                             Height,
                             0,
                             GL_RGB,
                             GL_UNSIGNED_BYTE,
                             +scaled_Image (1, 1).Red'Address);
               openGL.Errors.log;  -- tbd: only for debug.
            end;
         else
            glTexImage2D (GL_TEXTURE_2D,
                          0,
                          GL_RGB,
                          Width,
                          Height,
                          0,
                          GL_RGB,
                          GL_UNSIGNED_BYTE,
                          +the_Image (1, 1).Red'Address);
            openGL.Errors.log;  -- tbd: only for debug.
         end if;

         if use_Mipmaps then
            glGenerateMipmap (GL_TEXTURE_2D);
         end if;

         openGL.Errors.log;  -- tbd: only for debug.
      end;
   end set_Image;



   procedure set_Image (Self : in out Object;   To          : in openGL.lucid_Image;
                                                use_Mipmaps : in Boolean           := True)
   is
      use type Real;

      the_Image   : openGL.lucid_Image renames To;

      min_Width   : constant Positive := the_Image'Length (2);
      min_Height  : constant Positive := the_Image'Length (1);

      Width       : constant GLsizei  := power_of_2_Ceiling (min_Width);
      Height      : constant GLsizei  := power_of_2_Ceiling (min_Height);

      Size_width  : constant Size     := to_Size (min_Width);
      Size_height : constant Size     := to_Size (min_Height);

   begin
      Self.is_Transparent := True;

      Self.Size_width  := Size_width;
      Self.Size_height := Size_height;

      enable (Self);

      glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      openGL.Errors.log;  -- tbd: only for debug.

      if min_Width /= Positive (Width)
        or else min_Height /= Positive (Height)
      then
         declare
            scaled_Image : aliased openGL.lucid_Image (1 .. Index_t (Height),
                                                       1 .. Index_t (Width));
         begin
            gluScaleImage (GL_RGBA,
                           GLint (min_Width),
                           GLint (min_Height),
                           GL_UNSIGNED_BYTE,
                           the_Image (1, 1).Primary.Red'Address,
                           Width,
                           Height,
                           GL_UNSIGNED_BYTE,
                           scaled_Image (1, 1).Primary.Red'Address);
            openGL.Errors.log;  -- tbd: only for debug.

            glTexImage2D (GL_TEXTURE_2D,
                          0,
                          GL_RGBA,
                          Width,
                          Height,
                          0,
                          GL_RGBA,
                          GL_UNSIGNED_BYTE,
                          +scaled_Image (1, 1).Primary.Red'Address);
            openGL.Errors.log;  -- tbd: only for debug.
         end;
      else
         glTexImage2D (GL_TEXTURE_2D,
                       0,
                       GL_RGBA,
                       Width,
                       Height,
                       0,
                       GL_RGBA,
                       GL_UNSIGNED_BYTE,
                       +the_Image (1, 1).Primary.Red'Address);
         openGL.Errors.log;  -- tbd: only for debug.
      end if;

      if use_Mipmaps then
         glGenerateMipmap (GL_TEXTURE_2D);
      end if;

      openGL.Errors.log;  -- tbd: only for debug.
   end set_Image;



   function is_Transparent (Self : in Object) return Boolean
   is
   begin
      return Self.is_Transparent;
   end is_Transparent;



   procedure enable (Self : in Object)
   is
      use type GL.GLuint;
      pragma Assert (Self.Name > 0);
   begin
      glBindTexture (GL.GL_TEXTURE_2D, Self.Name);
   end enable;




   function to_Size (From : in Positive) return Size
   is
   begin
      if    From <=         2 then   return s2;
      elsif From <=         4 then   return s4;
      elsif From <=         8 then   return s8;
      elsif From <=        16 then   return s16;
      elsif From <=        32 then   return s32;
      elsif From <=        64 then   return s64;
      elsif From <=       128 then   return s128;
      elsif From <=       256 then   return s256;
      elsif From <=       512 then   return s512;
      elsif From <=      1024 then   return s1024;
      elsif From <=  2 * 1024 then   return s2048;
      elsif From <=  4 * 1024 then   return s4096;
      elsif From <=  8 * 1024 then   return s8192;
      elsif From <= 16 * 1024 then   return s16384;
      elsif From <= 32 * 1024 then   return s32768;
      end if;

      raise Constraint_Error with   "requested txture size too large:"
                                  & Positive'Image (From);
   end to_Size;



   function to_Integer (From : in Size) return Integer is
   begin
      case From is
      when Unknown => raise Constraint_Error;
      when s2      => return     2;
      when s4      => return     4;
      when s8      => return     8;
      when s16     => return    16;
      when s32     => return    32;
      when s64     => return    64;
      when s128    => return   128;
      when s256    => return   256;
      when s512    => return   512;
      when s1024   => return  1024;
      when s2048   => return  2048;
      when s4096   => return  4096;
      when s8192   => return  8192;
      when s16384  => return 16384;
      when s32768  => return 32768;
      end case;
   end to_Integer;



   function power_of_2_Ceiling (From : in Positive) return GL.GLsizei
   is
      use type GL.GLsizei;
   begin
      if    From <=         2 then   return         2;
      elsif From <=         4 then   return         4;
      elsif From <=         8 then   return         8;
      elsif From <=        16 then   return        16;
      elsif From <=        32 then   return        32;
      elsif From <=        64 then   return        64;
      elsif From <=       128 then   return       128;
      elsif From <=       256 then   return       256;
      elsif From <=       512 then   return       512;
      elsif From <=      1024 then   return      1024;
      elsif From <=  2 * 1024 then   return  2 * 1024;
      elsif From <=  4 * 1024 then   return  4 * 1024;
      elsif From <=  8 * 1024 then   return  8 * 1024;
      elsif From <= 16 * 1024 then   return 16 * 1024;
      elsif From <= 32 * 1024 then   return 32 * 1024;
      end if;

      raise Constraint_Error with "texture size too large";
   end power_of_2_Ceiling;



   function Size_width (Self : in Object) return Size
   is
   begin
      return Self.Size_width;
   end Size_width;



   function Size_height (Self : in Object) return Size
   is
   begin
      return Self.Size_height;
   end Size_height;



   --  Pool
   --

   --  tbd: add texture properties as 'in' parameters to handle different
   --  types of textures.
   --
   --     function new_Texture (From : access Pool;   min_Width  : in Positive;
   --                                                 min_Height : in Positive) return Object
   --     is
   --        the_Texture : aliased Object;
   --
   --        Size_width  : constant Size := to_Size (min_Width);
   --        Size_height : constant Size := to_Size (min_Height);
   --
   --        unused_texture_List : pool_texture_List_view :=
   --  from.unused_Textures_for_size (Size_width, Size_height);
   --     begin
   --        if unused_texture_List = null then
   --           unused_texture_List                                     := new
   --  pool_texture_List;
   --           from.unused_Textures_for_size (Size_width, Size_height) :=
   --  unused_texture_List;
   --        end if;
   --
   --        -- search for existing, but unused, object.
   --        --
   --        if unused_texture_List.Last > 0 then -- an existing unused
   --  texture has been found
   --           the_Texture              := unused_texture_List.Textures
   --(unused_texture_List.Last);
   --           unused_texture_List.Last := unused_texture_List.Last - 1;
   --
   --           enable (the_Texture);
   --
   --           gltexImage2D  (GL_TEXTURE_2D,  0,  GL_RGBA,
   --                           power_of_2_Ceiling (min_Width),
   --  power_of_2_Ceiling (min_Height),
   --                           0,
   --                           GL_RGBA, GL_UNSIGNED_BYTE,
   --  system.null_Address);    -- nb: actual image is not initialised.
   --        else
   --           -- no existing, unused texture found, so create a new one.
   --           --
   --           the_Texture.Size_width  := Size_width;
   --           the_Texture.Size_height := Size_height;
   --
   --           the_Texture.Pool := From.all'access;
   --
   --
   --           the_Texture.Name := new_texture_Name;
   --           enable (the_Texture);
   --
   --           glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);
   --           glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
   --  GL_CLAMP_TO_EDGE);
   --           glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
   --  GL_CLAMP_TO_EDGE);
   --
   --           glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
   --  GL_LINEAR);
   --           glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
   --  GL_LINEAR);
   --
   --           glTexEnvi ( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   --
   --
   --           gltexImage2D  (gl_TEXTURE_2D,  0,  gl_RGBA,
   --                           power_of_2_Ceiling (min_Width),
   --  power_of_2_Ceiling (min_Height),
   --                           0,
   --                           GL_RGBA, GL_UNSIGNED_BYTE,
   --  system.null_Address);    -- nb: actual image is not initialised.
   --        end if;
   --
   --        return the_Texture;
   --     end;

   --     procedure free (Self : in out Pool;   the_Texture : in Object)
   --     is
   --        use type texture_Name;
   --     begin
   --        if the_Texture.Name = 0 then
   --           return;
   --        end if;
   --
   --        declare
   --           unused_texture_List : constant pool_texture_List_view :=
   --    Self.unused_Textures_for_size (the_Texture.Size_width,
   --    the_Texture.Size_height);
   --        begin
   --           unused_texture_List.Last                                :=
   --  unused_texture_List.Last + 1;
   --           unused_texture_List.Textures (unused_texture_List.Last) :=
   --  the_Texture;
   --        end;
   --     end;

   --     procedure vacuum (Self : in out Pool)
   --     is
   --     begin
   --
   --        for each_Width in self.unused_Textures_for_size'range (1) loop
   --           for each_Height in self.unused_Textures_for_size'range (2) loop
   --              declare
   --                 unused_texture_List : constant pool_texture_List_view :=
   --  Self.unused_Textures_for_size (each_Width, each_Height);
   --              begin
   --                 if unused_texture_List /= null then
   --
   --                    for Each in 1 .. unused_texture_List.Last loop
   --                       free (unused_texture_List.Textures (Each).Name);
   --                    end loop;
   --
   --                    unused_texture_List.Last := 0;
   --                 end if;
   --              end;
   --           end loop;
   --        end loop;
   --
   --     end;



end opengl.Texture;
