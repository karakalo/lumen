with
     Interfaces.C,
     System;


package eGL
is
   use Interfaces;

   ---------
   --  Types
   --
   subtype void_Ptr         is System.Address;
   subtype Display          is System.Address;
   subtype NativeWindowType is Interfaces.C.unsigned_long;
   subtype NativePixmapType is Interfaces.C.unsigned_long;
   subtype EGLint           is Interfaces.Integer_32;
   subtype EGLBoolean       is Interfaces.C.unsigned;
   subtype EGLenum          is Interfaces.C.unsigned;
   subtype EGLConfig        is void_ptr;
   subtype EGLContext       is void_ptr;
   subtype EGLDisplay       is void_ptr;
   subtype EGLSurface       is void_ptr;
   subtype EGLClientBuffer  is void_ptr;

   type void_Ptr_array         is array (C.size_t range <>) of aliased void_Ptr;
   type Display_array          is array (C.size_t range <>) of aliased eGL.Display;
   type NativeWindowType_array is array (C.size_t range <>) of aliased eGL.NativeWindowType;
   type NativePixmapType_array is array (C.size_t range <>) of aliased eGL.NativePixmapType;
   type EGLint_array           is array (C.size_t range <>) of aliased eGL.EGLint;
   type EGLBoolean_array       is array (C.size_t range <>) of aliased eGL.EGLBoolean;
   type EGLenum_array          is array (C.size_t range <>) of aliased eGL.EGLenum;
   type EGLConfig_array        is array (C.size_t range <>) of aliased eGL.EGLConfig;
   type EGLContext_array       is array (C.size_t range <>) of aliased eGL.EGLContext;
   type EGLDisplay_array       is array (C.size_t range <>) of aliased eGL.EGLDisplay;
   type EGLSurface_array       is array (C.size_t range <>) of aliased eGL.EGLSurface;
   type EGLClientBuffer_array  is array (C.size_t range <>) of aliased eGL.EGLClientBuffer;


   -------------
   --  Constants
   --
   EGL_VERSION_1_0                 : constant := 1;
   EGL_VERSION_1_1                 : constant := 1;
   EGL_VERSION_1_2                 : constant := 1;
   EGL_VERSION_1_3                 : constant := 1;
   EGL_VERSION_1_4                 : constant := 1;

   EGL_FALSE                       : constant := 0;
   EGL_TRUE                        : constant := 1;

   EGL_SUCCESS                     : constant := 16#3000#;
   EGL_NOT_INITIALIZED             : constant := 16#3001#;

   EGL_BAD_ACCESS                  : constant := 16#3002#;
   EGL_BAD_ALLOC                   : constant := 16#3003#;
   EGL_BAD_ATTRIBUTE               : constant := 16#3004#;
   EGL_BAD_CONFIG                  : constant := 16#3005#;
   EGL_BAD_CONTEXT                 : constant := 16#3006#;
   EGL_BAD_CURRENT_SURFACE         : constant := 16#3007#;
   EGL_BAD_DISPLAY                 : constant := 16#3008#;
   EGL_BAD_MATCH                   : constant := 16#3009#;
   EGL_BAD_NATIVE_PIXMAP           : constant := 16#300a#;
   EGL_BAD_NATIVE_WINDOW           : constant := 16#300b#;
   EGL_BAD_PARAMETER               : constant := 16#300c#;
   EGL_BAD_SURFACE                 : constant := 16#300d#;

   EGL_CONTEXT_LOST                : constant := 16#300e#;

   EGL_BUFFER_SIZE                 : constant := 16#3020#;
   EGL_ALPHA_SIZE                  : constant := 16#3021#;
   EGL_BLUE_SIZE                   : constant := 16#3022#;
   EGL_GREEN_SIZE                  : constant := 16#3023#;
   EGL_RED_SIZE                    : constant := 16#3024#;
   EGL_DEPTH_SIZE                  : constant := 16#3025#;
   EGL_STENCIL_SIZE                : constant := 16#3026#;

   EGL_CONFIG_CAVEAT               : constant := 16#3027#;
   EGL_CONFIG_ID                   : constant := 16#3028#;

   EGL_LEVEL                       : constant := 16#3029#;

   EGL_MAX_PBUFFER_HEIGHT          : constant := 16#302a#;
   EGL_MAX_PBUFFER_PIXELS          : constant := 16#302b#;
   EGL_MAX_PBUFFER_WIDTH           : constant := 16#302c#;

   EGL_NATIVE_RENDERABLE           : constant := 16#302d#;
   EGL_NATIVE_VISUAL_ID            : constant := 16#302e#;
   EGL_NATIVE_VISUAL_TYPE          : constant := 16#302f#;

   EGL_PRESERVED_RESOURCES         : constant := 16#3030#;

   EGL_SAMPLES                     : constant := 16#3031#;
   EGL_SAMPLE_BUFFERS              : constant := 16#3032#;

   EGL_SURFACE_TYPE                : constant := 16#3033#;

   EGL_TRANSPARENT_TYPE            : constant := 16#3034#;
   EGL_TRANSPARENT_BLUE_VALUE      : constant := 16#3035#;
   EGL_TRANSPARENT_GREEN_VALUE     : constant := 16#3036#;
   EGL_TRANSPARENT_RED_VALUE       : constant := 16#3037#;

   EGL_NONE                        : constant := 16#3038#;

   EGL_BIND_TO_TEXTURE_RGB         : constant := 16#3039#;
   EGL_BIND_TO_TEXTURE_RGBA        : constant := 16#303a#;

   EGL_MIN_SWAP_INTERVAL           : constant := 16#303b#;
   EGL_MAX_SWAP_INTERVAL           : constant := 16#303c#;

   EGL_LUMINANCE_SIZE              : constant := 16#303d#;
   EGL_ALPHA_MASK_SIZE             : constant := 16#303e#;

   EGL_COLOR_BUFFER_TYPE           : constant := 16#303f#;
   EGL_RENDERABLE_TYPE             : constant := 16#3040#;

   EGL_MATCH_NATIVE_PIXMAP         : constant := 16#3041#;
   EGL_CONFORMANT                  : constant := 16#3042#;

   EGL_SLOW_CONFIG                 : constant := 16#3050#;
   EGL_NON_CONFORMANT_CONFIG       : constant := 16#3051#;

   EGL_TRANSPARENT_RGB             : constant := 16#3052#;
   EGL_RGB_BUFFER                  : constant := 16#308e#;
   EGL_LUMINANCE_BUFFER            : constant := 16#308f#;

   EGL_NO_TEXTURE                  : constant := 16#305c#;
   EGL_TEXTURE_RGB                 : constant := 16#305d#;
   EGL_TEXTURE_RGBA                : constant := 16#305e#;
   EGL_TEXTURE_2D                  : constant := 16#305f#;

   EGL_PBUFFER_BIT                 : constant := 16#1#;
   EGL_PIXMAP_BIT                  : constant := 16#2#;
   EGL_WINDOW_BIT                  : constant := 16#4#;
   EGL_VG_COLORSPACE_LINEAR_BIT    : constant := 16#20#;
   EGL_VG_ALPHA_FORMAT_PRE_BIT     : constant := 16#40#;
   EGL_MULTISAMPLE_RESOLVE_BOX_BIT : constant := 16#200#;
   EGL_SWAP_BEHAVIOR_PRESERVED_BIT : constant := 16#400#;

   EGL_OPENGL_ES_BIT               : constant := 16#1#;
   EGL_OPENVG_BIT                  : constant := 16#2#;
   EGL_OPENGL_ES2_BIT              : constant := 16#4#;
   EGL_OPENGL_BIT                  : constant := 16#8#;

   EGL_VENDOR                      : constant := 16#3053#;
   EGL_VERSION                     : constant := 16#3054#;
   EGL_EXTENSIONS                  : constant := 16#3055#;
   EGL_CLIENT_APIS                 : constant := 16#308d#;

   EGL_HEIGHT                      : constant := 16#3056#;
   EGL_WIDTH                       : constant := 16#3057#;

   EGL_LARGEST_PBUFFER             : constant := 16#3058#;

   EGL_TEXTURE_FORMAT              : constant := 16#3080#;
   EGL_TEXTURE_TARGET              : constant := 16#3081#;

   EGL_MIPMAP_TEXTURE              : constant := 16#3082#;
   EGL_MIPMAP_LEVEL                : constant := 16#3083#;

   EGL_RENDER_BUFFER               : constant := 16#3086#;

   EGL_VG_COLORSPACE               : constant := 16#3087#;
   EGL_VG_ALPHA_FORMAT             : constant := 16#3088#;

   EGL_HORIZONTAL_RESOLUTION       : constant := 16#3090#;
   EGL_VERTICAL_RESOLUTION         : constant := 16#3091#;

   EGL_PIXEL_ASPECT_RATIO          : constant := 16#3092#;
   EGL_SWAP_BEHAVIOR               : constant := 16#3093#;
   EGL_MULTISAMPLE_RESOLVE         : constant := 16#3099#;

   EGL_BACK_BUFFER                 : constant := 16#3084#;
   EGL_SINGLE_BUFFER               : constant := 16#3085#;

   EGL_VG_COLORSPACE_sRGB          : constant := 16#3089#;
   EGL_VG_COLORSPACE_LINEAR        : constant := 16#308a#;
   EGL_VG_ALPHA_FORMAT_NONPRE      : constant := 16#308b#;
   EGL_VG_ALPHA_FORMAT_PRE         : constant := 16#308c#;

   EGL_DISPLAY_SCALING             : constant := 10000;

   EGL_BUFFER_PRESERVED            : constant := 16#3094#;
   EGL_BUFFER_DESTROYED            : constant := 16#3095#;

   EGL_OPENVG_IMAGE                : constant := 16#3096#;

   EGL_CONTEXT_CLIENT_TYPE         : constant := 16#3097#;
   EGL_CONTEXT_CLIENT_VERSION      : constant := 16#3098#;

   EGL_MULTISAMPLE_RESOLVE_DEFAULT : constant := 16#309a#;
   EGL_MULTISAMPLE_RESOLVE_BOX     : constant := 16#309b#;

   EGL_OPENGL_ES_API               : constant := 16#30a0#;
   EGL_OPENVG_API                  : constant := 16#30a1#;
   EGL_OPENGL_API                  : constant := 16#30a2#;

   EGL_DRAW                        : constant := 16#3059#;
   EGL_READ                        : constant := 16#305a#;

   EGL_CORE_NATIVE_ENGINE          : constant := 16#305b#;

   EGL_COLORSPACE                  : constant := 16#3087#;
   EGL_ALPHA_FORMAT                : constant := 16#3088#;
   EGL_COLORSPACE_sRGB             : constant := 16#3089#;
   EGL_COLORSPACE_LINEAR           : constant := 16#308a#;
   EGL_ALPHA_FORMAT_NONPRE         : constant := 16#308b#;
   EGL_ALPHA_FORMAT_PRE            : constant := 16#308c#;

end eGL;
