with
     openGL.Display,
     openGL.Screen,
     Glx;

private
with
     Glx.Pointers;


package opengl.surface_Profile
--
--  Models an openGL surface profile.
--
is

   type Item  is tagged private;
   type View  is access all Item'Class;

   type Items is array (Positive range <>) of Item;



   --  Surface Quality
   --
   Irrelevant : constant Natural := Natural'Last;

   type color_Buffer is
      record
         Bits_red        : Natural := Irrelevant;
         Bits_green      : Natural := Irrelevant;
         Bits_blue       : Natural := Irrelevant;

         Bits_luminence  : Natural := Irrelevant;

         Bits_alpha      : Natural := Irrelevant;
         Bits_alpha_mask : Natural := Irrelevant;
      end record;

   function Image (Self : in color_Buffer) return String;


   type Qualities is
      record
         color_Buffer        : surface_Profile.color_Buffer;
         depth_buffer_Bits   : Natural                     := Irrelevant;
         stencil_buffer_Bits : Natural                     := Irrelevant;
      end record;

   default_Qualities : constant Qualities;

   function Image (Self : in Qualities) return String;



   --  Forge
   --
   desired_Qualitites_unavailable : exception;

   procedure define    (Self : in out Item;   the_Display : access opengl.Display.item'Class;
                                              Screen      : access opengl.Screen .item'Class;
                                              Desired     : in     Qualities                := default_Qualities);

   function  fetch_All (the_Display : access opengl.Display.item'class) return surface_Profile.items;



   --  Attributes
   --
   function  Quality    (Self : in Item) return Qualities;
   function  get_Visual (Self : in Item) return access GLX.XVisualInfo;



private

   type Item is tagged
      record
         glx_Config :        GLX.GLXFBConfig;
         Display    : access opengl.Display.item'Class;
         Visual     : access GLX.XVisualInfo;
      end record;

   default_Qualities : constant Qualities := (color_Buffer        => (Bits_red   => 8,
                                                                      Bits_green => 8,
                                                                      Bits_blue  => 8,

                                                                      Bits_luminence  => Irrelevant,

                                                                      Bits_alpha      => Irrelevant,
                                                                      Bits_alpha_mask => Irrelevant),
                                              depth_buffer_Bits   => 24,
                                              stencil_buffer_Bits => Irrelevant);

--     default_Qualities : constant Qualities := (color_Buffer        => (Bits_red   => 5,
--                                                                        Bits_green => 6,
--                                                                        Bits_blue  => 5,
--
--                                                                        Bits_luminence  => Irrelevant,
--
--                                                                        Bits_alpha      => Irrelevant,
--                                                                        Bits_alpha_mask => Irrelevant),
--                                                depth_buffer_Bits   => 16,
--                                                stencil_buffer_Bits => Irrelevant
--                                               );

end opengl.surface_Profile;
