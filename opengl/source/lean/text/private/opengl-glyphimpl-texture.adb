with
     openGL.Palette,

     GL.lean,
     GL.Pointers,

     freetype_c.Binding,
     freetype_c.FT_Bitmap,

     Interfaces.C;


package body openGL.GlyphImpl.Texture
is

   -----------
   --  Globals
   --

   activeTextureID : openGL.texture.texture_Name;
   --
   --  The texture index of the currently active texture
   --
   --  We keep track of the currently active texture to try to reduce the
   --  number of texture bind operations.


   procedure ResetActiveTexture
   is
   begin
      activeTextureID := 0;
   end ResetActiveTexture;


   ---------
   --  Forge
   --

   function new_GlyphImpl (glyth_Slot       : in freetype_c.FT_GlyphSlot.item;
                           texture_id       : in openGL.Texture.texture_Name;
                           xOffset, yOffset : in Integer;
                           width, height    : in Integer) return GlyphImpl.Texture.view
   is
      use      freetype_c,
               freetype_c.Binding,
               GL;
      use type Real,
               interfaces.c.unsigned,
               GLint;

      Self : constant GlyphImpl.Texture.view := new GlyphImpl.Texture.item;
   begin
      Self.define (glyth_Slot);

      Self.destWidth   := 0;
      Self.destHeight  := 0;
      Self.glTextureID := texture_id;

      --      /* FIXME: need to propagate the render mode all the way down to
      --       * here in order to get FT_RENDER_MODE_MONO aliased fonts.
      --       */

      Self.err := error_Kind'Val (FT_Render_Glyph (glyth_Slot,
                                                   FT_RENDER_MODE_NORMAL));

      if Self.err /= no_Error then
         raise openGL.Error with "FT_Render_Glyph failed with error code: " & error_Kind'Image (Self.err);
      end if;

      if FT_GlyphSlot_Get_Format (glyth_Slot) /= get_FT_GLYPH_FORMAT_BITMAP then
         raise openGL.Error with "Glyph is not a bitmap format.";
      end if;

      declare
         use GL.lean,
             GL.Pointers;
         bitmap : constant freetype_c.FT_Bitmap.item := FT_GlyphSlot_Get_Bitmap (glyth_Slot);
      begin
         Self.destWidth  := bitmap.width;
         Self.destHeight := bitmap.rows;

         if         Self.destWidth  /= 0
           and then Self.destHeight /= 0
         then
            --          glPushClientAttrib(GL_CLIENT_PIXEL_STORE_BIT);
            --          glPixelStorei(GL_UNPACK_LSB_FIRST, GL_FALSE);
            --          glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
            glPixelStorei   (GL_UNPACK_ALIGNMENT, 1);

            glBindTexture   (GL_TEXTURE_2D,    Self.glTextureID);
            glTexSubImage2D (GL_TEXTURE_2D,    0,
                             GLint (xOffset),  GLint (yOffset),
                             Self.destWidth,   Self.destHeight,
                             GL_ALPHA,   -- GL_RGB,
                             GL_UNSIGNED_BYTE, to_GLvoid_access (bitmap.buffer));
            --          glPopClientAttrib();
         end if;
      end;

      --        0
      --        +----+
      --        |    |
      --        |    |
      --        |    |
      --        +----+
      --             1

      Self.uv (1).S := Real (xOffset) / Real (width);
      Self.uv (1).T := Real (yOffset) / Real (height);

      Self.uv (2).S := Real (GLint (xOffset) + Self.destWidth)  / Real (width);
      Self.uv (2).T := Real (GLint (yOffset) + Self.destHeight) / Real (height);

      Self.corner   := (Real (FT_GlyphSlot_Get_bitmap_left (glyth_Slot)),
                        Real (FT_GlyphSlot_Get_bitmap_top  (glyth_Slot)),
                        0.0);
      declare
         use openGL.Primitive;
         the_Indices : constant openGL.Indices := (1, 2, 3, 4);
      begin
         Self.Primitive := Primitive.indexed.new_Primitive (triangle_Fan,  the_Indices);
      end;

--        Self.Geometry := opengl.Geometry.lit_colored_textured.new_Geometry;
--        Self.Geometry.add (Self.Primitive);
--        Self.Geometry.Texture_is (opengl.texture.to_Texture (Self.glTextureID));

      return Self;
   end new_GlyphImpl;



   --------------
   --  Attributes
   --

   function Quad (Self : in Item;   Pen : in Vector_3) return Quad_t
   is
      use type openGL.Real;

      dx       : constant         Real   := Real'Floor (Pen (1) + Self.corner (1));
      dy       : constant         Real   := Real'Floor (Pen (2) + Self.corner (2));

      the_Quad : aliased constant Quad_t := (NW      => (site   => (dx,
                                                                    dy,
                                                                    0.0),
                                                         coords => (s => Self.uv (1).S,
                                                                    t => Self.uv (1).T)),

                                             SW      => (site   => (dx,
                                                                    dy - Real (Self.destHeight),
                                                                    0.0),
                                                         coords => (s => Self.uv (1).S,
                                                                    t => Self.uv (2).T)),

                                             SE      => (site   => (dx + Real (Self.destWidth),
                                                                    dy - Real (Self.destHeight),
                                                                    0.0),
                                                         coords => (s => Self.uv (2).S,
                                                                    t => Self.uv (2).T)),

                                             NE      => (site   => (dx + Real (Self.destWidth),
                                                                    dy,
                                                                    0.0),
                                                         coords => (s => Self.uv (2).S,
                                                                    t => Self.uv (1).T)),

                                             Advance => Self.advance);
   begin
      return the_Quad;
   end Quad;



   --------------
   --  Operations
   --

   function renderImpl (Self : in Item;   Pen        : in Vector_3;
                                          renderMode : in Integer) return Vector_3
   is
      pragma Unreferenced (renderMode);

      use      GL, GL.lean;
      use type openGL.Texture.texture_Name,
               openGL.Real;

      dx : constant Real := Real'Floor (Pen (1) + Self.corner (1));
      dy : constant Real := Real'Floor (Pen (2) + Self.corner (2));
   begin
--        if activeTextureID /= Self.glTextureID then
--           glBindTexture (GL_TEXTURE_2D, GLuint (Self.glTextureID));
--           activeTextureID := Self.glTextureID;
--        end if;

      declare
         use openGL.Palette;

         the_Vertices : aliased Geometry.lit_colored_textured.Vertex_array
           := (1 => (site   => (dx,
                                dy,
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (1).S,
                                t => Self.uv (1).T)),

               2 => (site   => (dx,
                                dy - Real (Self.destHeight),
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (1).S,
                                t => Self.uv (2).T)),

               3 => (site   => (dx + Real (Self.destWidth),
                                dy - Real (Self.destHeight),
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (2).S,
                                t => Self.uv (2).T)),

               4 => (site   => (dx + Real (Self.destWidth),
                                dy,
                                0.0),
                     normal => (0.0, 0.0, 1.0),
                     color  => (White, Opaque),
                     coords => (s => Self.uv (2).S,
                                t => Self.uv (1).T)));
         --           the_Light                : openGL.Light.directional.item;
      begin
         null;
--           the_Light.Site_is  ((0.0, 10.0, 10.0), openGL.Math.Identity_3x3); --inverse_view_Transform);
--           the_Light.Color_is (ambient  => (0.7, 0.7, 0.7, 1.0),
--                               diffuse  => (1.0, 1.0, 1.0, 1.0),
--                               specular => (1.0, 1.0, 1.0, 1.0));
--
--  --           Self.Geometry.Texture_is (Self.
--           Self.Geometry.Vertices_are (the_Vertices'access);
--           Self.Geometry.Bounds_are   ((lower => (dx,
--                                                  dy - Real (Self.destHeight),
--                                                  0.0),
--                                        upper => (dx + Real (Self.destWidth),
--                                                  dy,
--                                                  0.0)));
--           Self.Geometry.Program.enable;
--
--           Self.Geometry.Program.mvp_Matrix_is               (opengl.math.Identity_4x4);
--           Self.Geometry.Program.inverse_modelview_Matrix_is (opengl.math.Identity_3x3);
--           Self.Geometry.Program.directional_Light_is        (the_Light);
--
--           Self.Geometry.render;

         --      glBegin(GL_QUADS);
         --          glTexCoord2f(uv[0].Xf(), uv[0].Yf());
         --          glVertex2f(dx, dy);
         --
         --          glTexCoord2f(uv[0].Xf(), uv[1].Yf());
         --          glVertex2f(dx, dy - destHeight);
         --
         --          glTexCoord2f(uv[1].Xf(), uv[1].Yf());
         --          glVertex2f(dx + destWidth, dy - destHeight);
         --
         --          glTexCoord2f(uv[1].Xf(), uv[0].Yf());
         --          glVertex2f(dx + destWidth, dy);
         --      glEnd();

      end;


      return Self.advance;
   end renderImpl;


end openGL.GlyphImpl.Texture;
