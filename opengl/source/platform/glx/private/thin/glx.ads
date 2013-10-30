with
     Interfaces.C.Strings,
     System;

package GLX
is
   use Interfaces;


   ---------
   --  Types
   --


   -- XEventQueueOwner
   --
   type XEventQueueOwner is  (nil);

   for  XEventQueueOwner use (nil => 0);
   pragma Convention (C, XEventQueueOwner);

   type XEventQueueOwner_Pointer  is access all XEventQueueOwner;

   type XEventQueueOwner_array    is array (C.size_t range <>) of aliased XEventQueueOwner;
   type XEventQueueOwner_Pointers is array (C.size_t range <>) of aliased XEventQueueOwner_Pointer;


   -- XEventQueueOwner_Pointer_Pointer
   --
   type XEventQueueOwner_Pointer_Pointer is access all GLX.XEventQueueOwner_Pointer;


   -- VisualID
   --
   subtype VisualID       is Interfaces.C.unsigned_long;
   type    VisualID_array is array (Interfaces.C.size_t range <>) of aliased GLX.VisualID;


   -- XVisualInfo
   --
   subtype XVisualInfo       is system.Address;
   type    XVisualInfo_array is array (Interfaces.C.size_t range <>) of aliased GLX.XVisualInfo;


   -- Pixmap
   --
   subtype Pixmap       is system.Address;
   type    Pixmap_array is array (Interfaces.C.size_t range <>) of aliased GLX.Pixmap;


   -- Font
   --
   subtype Font    is system.Address;
   type Font_array is array (Interfaces.C.size_t range <>) of aliased GLX.Font;


   -- Window
   --
   subtype Window    is system.Address;
   type Window_array is array (Interfaces.C.size_t range <>) of aliased GLX.Window;


   -- Bool
   --
   subtype Bool       is Interfaces.C.int;
   type    Bool_array is array (Interfaces.C.size_t range <>) of aliased GLX.Bool;


   -- GLXcontextRec
   --
   subtype GLXcontextRec       is system.Address;
   type    GLXcontextRec_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXcontextRec;


   -- XID
   --
   subtype XID       is system.Address;
   type    XID_array is array (Interfaces.C.size_t range <>) of aliased GLX.XID;


   -- GLXPixmap
   --
   subtype GLXPixmap       is GLX.XID;
   type    GLXPixmap_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXPixmap;


   -- GLXDrawable
   --
   subtype GLXDrawable    is GLX.XID;
   type GLXDrawable_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXDrawable;


   -- p_a_GLXFBConfigRec
   --
   subtype p_a_GLXFBConfigRec       is system.Address;
   type    p_a_GLXFBConfigRec_array is array (Interfaces.C.size_t range <>) of aliased GLX.p_a_GLXFBConfigRec;


   -- GLXFBConfig
   --
   subtype GLXFBConfig       is GLX.p_a_GLXFBConfigRec;
   type    GLXFBConfig_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXFBConfig;


   -- GLXFBConfigID
   --
   subtype GLXFBConfigID       is GLX.XID;
   type    GLXFBConfigID_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXFBConfigID;


   -- GLXContextID
   --
   subtype GLXContextID    is GLX.XID;
   type GLXContextID_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXContextID;


   -- GLXWindow
   --
   subtype GLXWindow       is GLX.XID;
   type    GLXWindow_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXWindow;


   -- GLXPbuffer
   --
   subtype GLXPbuffer    is GLX.XID;
   type GLXPbuffer_array is array (Interfaces.C.size_t range <>) of aliased GLX.GLXPbuffer;



   -------------
   --  Constants
   --
   GLX_VERSION_1_1                 : constant := 1;
   GLX_VERSION_1_2                 : constant := 1;
   GLX_VERSION_1_3                 : constant := 1;
   GLX_VERSION_1_4                 : constant := 1;
   GLX_EXTENSION_NAME              : aliased constant C.Strings.chars_ptr := C.Strings.New_String ("GLX");
   GLX_USE_GL                      : constant := 1;
   GLX_BUFFER_SIZE                 : constant := 2;
   GLX_LEVEL                       : constant := 3;
   GLX_RGBA                        : constant := 4;
   GLX_DOUBLEBUFFER                : constant := 5;
   GLX_STEREO                      : constant := 6;
   GLX_AUX_BUFFERS                 : constant := 7;
   GLX_RED_SIZE                    : constant := 8;
   GLX_GREEN_SIZE                  : constant := 9;
   GLX_BLUE_SIZE                   : constant := 10;
   GLX_ALPHA_SIZE                  : constant := 11;
   GLX_DEPTH_SIZE                  : constant := 12;
   GLX_STENCIL_SIZE                : constant := 13;
   GLX_ACCUM_RED_SIZE              : constant := 14;
   GLX_ACCUM_GREEN_SIZE            : constant := 15;
   GLX_ACCUM_BLUE_SIZE             : constant := 16;
   GLX_ACCUM_ALPHA_SIZE            : constant := 17;
   GLX_BAD_SCREEN                  : constant := 1;
   GLX_BAD_ATTRIBUTE               : constant := 2;
   GLX_NO_EXTENSION                : constant := 3;
   GLX_BAD_VISUAL                  : constant := 4;
   GLX_BAD_CONTEXT                 : constant := 5;
   GLX_BAD_VALUE                   : constant := 6;
   GLX_BAD_ENUM                    : constant := 7;
   GLX_VENDOR                      : constant := 1;
   GLX_VERSION                     : constant := 2;
   GLX_EXTENSIONS                  : constant := 3;
   GLX_CONFIG_CAVEAT               : constant := 16#20#;
   GLX_DONT_CARE                   : constant := 16#ffffffff#;
   GLX_X_VISUAL_TYPE               : constant := 16#22#;
   GLX_TRANSPARENT_TYPE            : constant := 16#23#;
   GLX_TRANSPARENT_INDEX_VALUE     : constant := 16#24#;
   GLX_TRANSPARENT_RED_VALUE       : constant := 16#25#;
   GLX_TRANSPARENT_GREEN_VALUE     : constant := 16#26#;
   GLX_TRANSPARENT_BLUE_VALUE      : constant := 16#27#;
   GLX_TRANSPARENT_ALPHA_VALUE     : constant := 16#28#;
   GLX_WINDOW_BIT                  : constant := 16#1#;
   GLX_PIXMAP_BIT                  : constant := 16#2#;
   GLX_PBUFFER_BIT                 : constant := 16#4#;
   GLX_AUX_BUFFERS_BIT             : constant := 16#10#;
   GLX_FRONT_LEFT_BUFFER_BIT       : constant := 16#1#;
   GLX_FRONT_RIGHT_BUFFER_BIT      : constant := 16#2#;
   GLX_BACK_LEFT_BUFFER_BIT        : constant := 16#4#;
   GLX_BACK_RIGHT_BUFFER_BIT       : constant := 16#8#;
   GLX_DEPTH_BUFFER_BIT            : constant := 16#20#;
   GLX_STENCIL_BUFFER_BIT          : constant := 16#40#;
   GLX_ACCUM_BUFFER_BIT            : constant := 16#80#;
   GLX_NONE                        : constant := 16#8000#;
   GLX_SLOW_CONFIG                 : constant := 16#8001#;
   GLX_TRUE_COLOR                  : constant := 16#8002#;
   GLX_DIRECT_COLOR                : constant := 16#8003#;
   GLX_PSEUDO_COLOR                : constant := 16#8004#;
   GLX_STATIC_COLOR                : constant := 16#8005#;
   GLX_GRAY_SCALE                  : constant := 16#8006#;
   GLX_STATIC_GRAY                 : constant := 16#8007#;
   GLX_TRANSPARENT_RGB             : constant := 16#8008#;
   GLX_TRANSPARENT_INDEX           : constant := 16#8009#;
   GLX_VISUAL_ID                   : constant := 16#800b#;
   GLX_SCREEN                      : constant := 16#800c#;
   GLX_NON_CONFORMANT_CONFIG       : constant := 16#800d#;
   GLX_DRAWABLE_TYPE               : constant := 16#8010#;
   GLX_RENDER_TYPE                 : constant := 16#8011#;
   GLX_X_RENDERABLE                : constant := 16#8012#;
   GLX_FBCONFIG_ID                 : constant := 16#8013#;
   GLX_RGBA_TYPE                   : constant := 16#8014#;
   GLX_COLOR_INDEX_TYPE            : constant := 16#8015#;
   GLX_MAX_PBUFFER_WIDTH           : constant := 16#8016#;
   GLX_MAX_PBUFFER_HEIGHT          : constant := 16#8017#;
   GLX_MAX_PBUFFER_PIXELS          : constant := 16#8018#;
   GLX_PRESERVED_CONTENTS          : constant := 16#801b#;
   GLX_LARGEST_PBUFFER             : constant := 16#801c#;
   GLX_WIDTH                       : constant := 16#801d#;
   GLX_HEIGHT                      : constant := 16#801e#;
   GLX_EVENT_MASK                  : constant := 16#801f#;
   GLX_DAMAGED                     : constant := 16#8020#;
   GLX_SAVED                       : constant := 16#8021#;
   GLX_WINDOW                      : constant := 16#8022#;
   GLX_PBUFFER                     : constant := 16#8023#;
   GLX_PBUFFER_HEIGHT              : constant := 16#8040#;
   GLX_PBUFFER_WIDTH               : constant := 16#8041#;
   GLX_RGBA_BIT                    : constant := 16#1#;
   GLX_COLOR_INDEX_BIT             : constant := 16#2#;
   GLX_PBUFFER_CLOBBER_MASK        : constant := 16#8000000#;
   GLX_SAMPLE_BUFFERS              : constant := 16#186a0#;
   GLX_SAMPLES                     : constant := 16#186a1#;
   GLX_PbufferClobber              : constant := 0;
   GLX_BufferSwapComplete          : constant := 1;
   a_a_GLX_NUMBER_EVENTS           : constant := 17;
   GLX_ARB_render_texture          : constant := 1;
   GLX_EXT_texture_from_pixmap     : constant := 1;
   GLX_BIND_TO_TEXTURE_RGB_EXT     : constant := 16#20d0#;
   GLX_BIND_TO_TEXTURE_RGBA_EXT    : constant := 16#20d1#;
   GLX_BIND_TO_MIPMAP_TEXTURE_EXT  : constant := 16#20d2#;
   GLX_BIND_TO_TEXTURE_TARGETS_EXT : constant := 16#20d3#;
   GLX_Y_INVERTED_EXT              : constant := 16#20d4#;
   GLX_TEXTURE_FORMAT_EXT          : constant := 16#20d5#;
   GLX_TEXTURE_TARGET_EXT          : constant := 16#20d6#;
   GLX_MIPMAP_TEXTURE_EXT          : constant := 16#20d7#;
   GLX_TEXTURE_FORMAT_NONE_EXT     : constant := 16#20d8#;
   GLX_TEXTURE_FORMAT_RGB_EXT      : constant := 16#20d9#;
   GLX_TEXTURE_FORMAT_RGBA_EXT     : constant := 16#20da#;
   GLX_TEXTURE_1D_BIT_EXT          : constant := 16#1#;
   GLX_TEXTURE_2D_BIT_EXT          : constant := 16#2#;
   GLX_TEXTURE_RECTANGLE_BIT_EXT   : constant := 16#4#;
   GLX_TEXTURE_1D_EXT              : constant := 16#20db#;
   GLX_TEXTURE_2D_EXT              : constant := 16#20dc#;
   GLX_TEXTURE_RECTANGLE_EXT       : constant := 16#20dd#;
   GLX_FRONT_LEFT_EXT              : constant := 16#20de#;
   GLX_FRONT_RIGHT_EXT             : constant := 16#20df#;
   GLX_BACK_LEFT_EXT               : constant := 16#20e0#;
   GLX_BACK_RIGHT_EXT              : constant := 16#20e1#;
   GLX_FRONT_EXT                   : constant := 16#20de#;
   GLX_BACK_EXT                    : constant := 16#20e0#;
   GLX_AUX0_EXT                    : constant := 16#20e2#;
   GLX_AUX1_EXT                    : constant := 16#20e3#;
   GLX_AUX2_EXT                    : constant := 16#20e4#;
   GLX_AUX3_EXT                    : constant := 16#20e5#;
   GLX_AUX4_EXT                    : constant := 16#20e6#;
   GLX_AUX5_EXT                    : constant := 16#20e7#;
   GLX_AUX6_EXT                    : constant := 16#20e8#;
   GLX_AUX7_EXT                    : constant := 16#20e9#;
   GLX_AUX8_EXT                    : constant := 16#20ea#;
   GLX_AUX9_EXT                    : constant := 16#20eb#;

end GLX;
