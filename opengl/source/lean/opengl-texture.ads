with
    ada.unchecked_Conversion;

package openGL.Texture
--
--  Provides openGL textures.
--
is
   pragma Remote_Types;


   ---------
   --  Types
   --

   subtype texture_Name is GL.GLuint;     -- An openGL texture 'Name'.


   --  TexFormatEnm
   --
   type TexFormatEnm is (ALPHA,
                         RGB,
                         RGBA,
                         LUMINANCE,
                         LUMINANCE_ALPHA,
                         R3_G3_B2,
                         ALPHA4,
                         ALPHA8,
                         ALPHA12,
                         ALPHA16,
                         LUMINANCE4,
                         LUMINANCE8,
                         LUMINANCE12,
                         LUMINANCE16,
                         LUMINANCE4_ALPHA4,
                         LUMINANCE6_ALPHA2,
                         LUMINANCE8_ALPHA8,
                         LUMINANCE12_ALPHA4,
                         LUMINANCE12_ALPHA12,
                         LUMINANCE16_ALPHA16,
                         INTENSITY,
                         INTENSITY4,
                         INTENSITY8,
                         INTENSITY12,
                         INTENSITY16,
                         RGB4,
                         RGB5,
                         RGB8,
                         RGB10,
                         RGB12,
                         RGB16,
                         RGBA2,
                         RGBA4,
                         RGB5_A1,
                         RGBA8,
                         RGB10_A2,
                         RGBA12,
                         RGBA16,
                         BGR,
                         BGRA);

   function to_GL (Self : in TexFormatEnm) return GL.GLenum;


   --  TexPixelFormatEnm
   --
   type TexPixelFormatEnm is (COLOR_INDEX,
                              RED,
                              GREEN,
                              BLUE,
                              ALPHA,
                              RGB,
                              RGBA,
                              LUMINANCE,
                              LUMINANCE_ALPHA);

   function to_GL (Self : in TexPixelFormatEnm) return GL.GLenum;


   --  Size
   --
   type Size is (Unknown,
                 s2,
                 s4,
                 s8,
                 s16,
                 s32,
                 s64,
                 s128,
                 s256,
                 s512,
                 s1024,
                 s2048,
                 s4096,
                 s8192,
                 s16384,
                 s32768);

   function to_Size    (From : in Positive) return Size;
   function to_Integer (From : in Size)     return Integer;


   --  Object - an openGL texture 'object'
   --
   type Object  is tagged private;
   type Objects is array (Positive range <>) of Object;

   null_Object : constant Object;



   ----------
   --- Forge
   --

   function  to_Texture (Name        : in     texture_Name) return Object;

   function  to_Texture (min_Width,
                         min_Height  : in     Positive) return Object;

   function  to_Texture (the_Image   : in     openGL.Image;
                         use_Mipmaps : in     Boolean := True) return Object;

   function  to_Texture (the_Image   : in     openGL.lucid_Image;
                         use_Mipmaps : in     Boolean := True) return Object;

   procedure destroy    (Self        : in out Object);



   ---------------
   --- Attributes
   --

   procedure set_Name       (Self        : in out Object;   To : in texture_Name);
   function  Name           (Self        : in     Object)    return texture_Name;

   procedure enable         (Self        : in     Object);

   function  Size_width     (Self        : in     Object) return Size;
   function  Size_height    (Self        : in     Object) return Size;

   function  is_Transparent (Self        : in     Object) return Boolean;

   procedure set_Image      (Self        : in out Object;
                             To          : in     openGL.Image;
                             use_Mipmaps : in     Boolean := True);

   procedure set_Image      (Self        : in out Object;
                             To          : in     openGL.lucid_Image;
                             use_Mipmaps : in     Boolean := True);


   --  Pool
   --
   --  For rapid allocation/deallocation of texture objects.
   --

   --     type Pool is private;

   --     function new_Texture (From : access Pool;   min_Width  : in Positive;
   --                                                 min_Height : in Positive) return Object;
   --
   --  returns a texture object, whose width and height are powers of two,
   --  sufficient to contain the requested minimums.

   --     procedure free (Self : in out Pool;   the_Texture : in Object);
   --
   --  free's a texture, for future use.

   --     procedure vacuum (Self : in out Pool);
   --
   --  releases any allocated, but unused, texture objects.



   --- Utility
   --

   function power_of_2_Ceiling (From : in Positive) return GL.GLsizei;



private

   type Object is tagged
      record
         Name           : aliased texture_Name := 0;
         Size_width,
         Size_height    :         Size         := Unknown;
         is_Transparent :         Boolean;
         --           Pool           : access  textures.Pool;
      end record;



   for TexFormatEnm use
     (ALPHA               => 16#1906#,
      RGB                 => 16#1907#,
      RGBA                => 16#1908#,
      LUMINANCE           => 16#1909#,
      LUMINANCE_ALPHA     => 16#190A#,
      R3_G3_B2            => 16#2A10#,
      ALPHA4              => 16#803B#,
      ALPHA8              => 16#803C#,
      ALPHA12             => 16#803D#,
      ALPHA16             => 16#803E#,
      LUMINANCE4          => 16#803F#,
      LUMINANCE8          => 16#8040#,
      LUMINANCE12         => 16#8041#,
      LUMINANCE16         => 16#8042#,
      LUMINANCE4_ALPHA4   => 16#8043#,
      LUMINANCE6_ALPHA2   => 16#8044#,
      LUMINANCE8_ALPHA8   => 16#8045#,
      LUMINANCE12_ALPHA4  => 16#8046#,
      LUMINANCE12_ALPHA12 => 16#8047#,
      LUMINANCE16_ALPHA16 => 16#8048#,
      INTENSITY           => 16#8049#,
      INTENSITY4          => 16#804A#,
      INTENSITY8          => 16#804B#,
      INTENSITY12         => 16#804C#,
      INTENSITY16         => 16#804D#,
      RGB4                => 16#804F#,
      RGB5                => 16#8050#,
      RGB8                => 16#8051#,
      RGB10               => 16#8052#,
      RGB12               => 16#8053#,
      RGB16               => 16#8054#,
      RGBA2               => 16#8055#,
      RGBA4               => 16#8056#,
      RGB5_A1             => 16#8057#,
      RGBA8               => 16#8058#,
      RGB10_A2            => 16#8059#,
      RGBA12              => 16#805A#,
      RGBA16              => 16#805B#,
      BGR                 => 16#80E0#,
      BGRA                => 16#80E1#);

   for TexFormatEnm'Size use GL.GLenum'Size;



   for TexPixelFormatEnm use
     (COLOR_INDEX     => 16#1900#,
      RED             => 16#1903#,
      GREEN           => 16#1904#,
      BLUE            => 16#1905#,
      ALPHA           => 16#1906#,
      RGB             => 16#1907#,
      RGBA            => 16#1908#,
      LUMINANCE       => 16#1909#,
      LUMINANCE_ALPHA => 16#190A#);

   for TexPixelFormatEnm'Size use GL.GLenum'Size;



   --  Pool
   --
   --  Re-uses existing textures when possible for performance.

   --     type pool_texture_List is
   --        record
   --           Textures : Objects (1 .. 3000);
   --           Last     : Natural            := 0;
   --        end record;
   --
   --  --     type pool_texture_List_view is access all pool_texture_List;
   --
   --  --     type pool_texture_Lists_by_size is array (Size, Size) of pool_texture_List_view;
   --     type pool_texture_Lists_by_size is array (Size, Size) of access pool_texture_List;
   --
   --
   --
   --     type Pool is
   --        record
   --           unused_Textures_for_size : pool_texture_Lists_by_size;
   --        end record;



   null_Object : constant Object := (others => <>);



   function convert_1 is new Ada.Unchecked_Conversion (TexFormatEnm,      GL.GLenum);
   function convert_2 is new Ada.Unchecked_Conversion (TexPixelFormatEnm, GL.GLenum);

   function to_GL (Self : in TexFormatEnm)      return GL.GLenum renames convert_1;
   function to_GL (Self : in TexPixelFormatEnm) return GL.GLenum renames convert_2;

end opengl.Texture;
