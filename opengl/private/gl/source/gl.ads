with
     Interfaces.C,
     System;

with GL_Types;

package GL
--
--  Provides types, constants and functions common to all openGL profiles.
--
is
   pragma Pure;


   ---------
   --  Types
   --

   -- GLvoid
   --
   subtype GLvoid       is GL_Types.GLvoid;
   type    GLvoid_array is array (Interfaces.C.size_t range <>) of aliased GLvoid;


   -- GLenum
   --
   subtype GLenum       is GL_Types.GLenum;
   type    GLenum_array is array (Interfaces.C.size_t range <>) of aliased GLenum;

   -- GLboolean
   --
   subtype GLboolean       is GL_Types.GLboolean;
   type    GLboolean_array is array (Interfaces.C.size_t range <>) of aliased GLboolean;

   -- GLbitfield
   --
   subtype GLbitfield       is GL_Types.GLbitfield;
   type    GLbitfield_array is array (Interfaces.C.size_t range <>) of aliased GLbitfield;

   -- GLshort
   --
   subtype GLshort       is GL_Types.GLshort;
   type    GLshort_array is array (Interfaces.C.size_t range <>) of aliased GLshort;

   -- GLint
   --
   subtype GLint       is GL_Types.GLint;
   type    GLint_array is array (Interfaces.C.size_t range <>) of aliased GLint;

   -- GLsizei
   --
   subtype GLsizei       is GL_Types.GLsizei;
   type    GLsizei_array is array (Interfaces.C.size_t range <>) of aliased GLsizei;

   -- GLushort
   --
   subtype GLushort       is GL_Types.GLushort;
   type    GLushort_array is array (Interfaces.C.size_t range <>) of aliased GLushort;

   -- GLuint
   --
   subtype GLuint       is GL_Types.GLuint;
   type    GLuint_array is array (Interfaces.C.size_t range <>) of aliased GLuint;


   -- GLbyte
   --
   subtype GLbyte       is GL_Types.GLbyte;
   type    GLbyte_array is array (Interfaces.C.size_t range <>) of aliased GLbyte;


   -- GLubyte
   --
   subtype GLubyte       is GL_Types.GLubyte;
   type    GLubyte_array is array (Interfaces.C.size_t range <>) of aliased GLubyte;


   -- GLfloat
   --
   subtype GLfloat       is GL_Types.GLfloat;
   type    GLfloat_array is array (Interfaces.C.size_t range <>) of aliased GLfloat;


   -- GLclampf
   --
   subtype GLclampf       is GL_Types.GLclampf;
   type    GLclampf_array is array (Interfaces.C.size_t range <>) of aliased GLclampf;



   -------------
   --  Constants
   --

   --  ClearBufferMask
   GL_DEPTH_BUFFER_BIT   : constant := 16#100#;
   GL_STENCIL_BUFFER_BIT : constant := 16#400#;
   GL_COLOR_BUFFER_BIT   : constant := 16#4000#;

   --  Boolean
   GL_FALSE : constant := 0;
   GL_TRUE  : constant := 1;

   --  BeginMode
   GL_POINTS         : constant := 16#0#;
   GL_LINES          : constant := 16#1#;
   GL_LINE_LOOP      : constant := 16#2#;
   GL_LINE_STRIP     : constant := 16#3#;
   GL_TRIANGLES      : constant := 16#4#;
   GL_TRIANGLE_STRIP : constant := 16#5#;
   GL_TRIANGLE_FAN   : constant := 16#6#;

   --  BlendingFactorDest
   GL_ZERO                : constant := 0;
   GL_ONE                 : constant := 1;
   GL_ONE_MINUS_SRC_ALPHA : constant := 16#303#;

   --  BlendingFactorSrc
   GL_SRC_ALPHA          : constant := 16#302#;
   GL_SRC_ALPHA_SATURATE : constant := 16#308#;

   --   CullFaceMode
   GL_FRONT          : constant := 16#404#;
   GL_BACK           : constant := 16#405#;
   GL_FRONT_AND_BACK : constant := 16#408#;

   --   EnableCap
   GL_TEXTURE_2D          : constant := 16#de1#;
   GL_CULL_FACE           : constant := 16#b44#;
   GL_BLEND               : constant := 16#be2#;
   GL_STENCIL_TEST        : constant := 16#b90#;
   GL_DEPTH_TEST          : constant := 16#b71#;
   GL_SCISSOR_TEST        : constant := 16#c11#;
   GL_POLYGON_OFFSET_FILL : constant := 16#8037#;

   --  ErrorCode
   GL_NO_ERROR          : constant := 0;
   GL_INVALID_ENUM      : constant := 16#500#;
   GL_INVALID_VALUE     : constant := 16#501#;
   GL_INVALID_OPERATION : constant := 16#502#;
   GL_OUT_OF_MEMORY     : constant := 16#505#;

   --  FrontFaceDirection
   GL_CW  : constant := 16#900#;
   GL_CCW : constant := 16#901#;


   --  todo: As above, categorise and add category comment for the following ...
   --

   GL_LINE_WIDTH : constant := 16#b21#;

   GL_ALIASED_POINT_SIZE_RANGE : constant := 16#846d#;
   GL_ALIASED_LINE_WIDTH_RANGE : constant := 16#846e#;

   GL_CULL_FACE_MODE    : constant := 16#b45#;
   GL_FRONT_FACE        : constant := 16#b46#;
   GL_DEPTH_RANGE       : constant := 16#b70#;
   GL_DEPTH_WRITEMASK   : constant := 16#b72#;
   GL_DEPTH_CLEAR_VALUE : constant := 16#b73#;
   GL_DEPTH_FUNC        : constant := 16#b74#;

   GL_STENCIL_CLEAR_VALUE     : constant := 16#b91#;
   GL_STENCIL_FUNC            : constant := 16#b92#;
   GL_STENCIL_FAIL            : constant := 16#b94#;
   GL_STENCIL_PASS_DEPTH_FAIL : constant := 16#b95#;
   GL_STENCIL_PASS_DEPTH_PASS : constant := 16#b96#;
   GL_STENCIL_REF             : constant := 16#b97#;
   GL_STENCIL_VALUE_MASK      : constant := 16#b93#;
   GL_STENCIL_WRITEMASK       : constant := 16#b98#;

   GL_VIEWPORT    : constant := 16#ba2#;
   GL_SCISSOR_BOX : constant := 16#c10#;

   GL_COLOR_CLEAR_VALUE : constant := 16#c22#;
   GL_COLOR_WRITEMASK   : constant := 16#c23#;

   GL_UNPACK_ALIGNMENT : constant := 16#cf5#;
   GL_PACK_ALIGNMENT   : constant := 16#d05#;

   GL_MAX_TEXTURE_SIZE  : constant := 16#d33#;
   GL_MAX_VIEWPORT_DIMS : constant := 16#d3a#;

   GL_SUBPIXEL_BITS : constant := 16#d50#;
   GL_RED_BITS      : constant := 16#d52#;
   GL_GREEN_BITS    : constant := 16#d53#;
   GL_BLUE_BITS     : constant := 16#d54#;
   GL_ALPHA_BITS    : constant := 16#d55#;
   GL_DEPTH_BITS    : constant := 16#d56#;
   GL_STENCIL_BITS  : constant := 16#d57#;

   GL_POLYGON_OFFSET_UNITS  : constant := 16#2a00#;
   GL_POLYGON_OFFSET_FACTOR : constant := 16#8038#;

   GL_TEXTURE_BINDING_2D : constant := 16#8069#;

   GL_DONT_CARE : constant := 16#1100#;
   GL_FASTEST   : constant := 16#1101#;
   GL_NICEST    : constant := 16#1102#;

   GL_BYTE          : constant := 16#1400#;
   GL_UNSIGNED_BYTE : constant := 16#1401#;
   GL_INT           : constant := 16#1404#;
   GL_UNSIGNED_INT  : constant := 16#1405#;
   GL_FLOAT         : constant := 16#1406#;

   GL_ALPHA           : constant := 16#1906#;
   GL_RGB             : constant := 16#1907#;
   GL_RGBA            : constant := 16#1908#;
   GL_LUMINANCE       : constant := 16#1909#;
   GL_LUMINANCE_ALPHA : constant := 16#190a#;

   GL_NEVER    : constant := 16#200#;
   GL_LESS     : constant := 16#201#;
   GL_EQUAL    : constant := 16#202#;
   GL_LEQUAL   : constant := 16#203#;
   GL_GREATER  : constant := 16#204#;
   GL_NOTEQUAL : constant := 16#205#;
   GL_GEQUAL   : constant := 16#206#;
   GL_ALWAYS   : constant := 16#207#;

   GL_KEEP    : constant := 16#1e00#;
   GL_REPLACE : constant := 16#1e01#;
   GL_INCR    : constant := 16#1e02#;
   GL_DECR    : constant := 16#1e03#;

   GL_INVERT : constant := 16#150a#;

   GL_VENDOR     : constant := 16#1f00#;
   GL_RENDERER   : constant := 16#1f01#;
   GL_VERSION    : constant := 16#1f02#;
   GL_EXTENSIONS : constant := 16#1f03#;

   GL_NEAREST : constant := 16#2600#;
   GL_LINEAR  : constant := 16#2601#;

   GL_NEAREST_MIPMAP_NEAREST : constant := 16#2700#;
   GL_LINEAR_MIPMAP_NEAREST  : constant := 16#2701#;
   GL_NEAREST_MIPMAP_LINEAR  : constant := 16#2702#;
   GL_LINEAR_MIPMAP_LINEAR   : constant := 16#2703#;

   GL_TEXTURE_MAG_FILTER : constant := 16#2800#;
   GL_TEXTURE_MIN_FILTER : constant := 16#2801#;
   GL_TEXTURE_WRAP_S     : constant := 16#2802#;
   GL_TEXTURE_WRAP_T     : constant := 16#2803#;

   GL_TEXTURE0  : constant := 16#84c0#;
   GL_TEXTURE1  : constant := 16#84c1#;
   GL_TEXTURE2  : constant := 16#84c2#;
   GL_TEXTURE3  : constant := 16#84c3#;
   GL_TEXTURE4  : constant := 16#84c4#;
   GL_TEXTURE5  : constant := 16#84c5#;
   GL_TEXTURE6  : constant := 16#84c6#;
   GL_TEXTURE7  : constant := 16#84c7#;
   GL_TEXTURE8  : constant := 16#84c8#;
   GL_TEXTURE9  : constant := 16#84c9#;
   GL_TEXTURE10 : constant := 16#84ca#;
   GL_TEXTURE11 : constant := 16#84cb#;
   GL_TEXTURE12 : constant := 16#84cc#;
   GL_TEXTURE13 : constant := 16#84cd#;
   GL_TEXTURE14 : constant := 16#84ce#;
   GL_TEXTURE15 : constant := 16#84cf#;
   GL_TEXTURE16 : constant := 16#84d0#;
   GL_TEXTURE17 : constant := 16#84d1#;
   GL_TEXTURE18 : constant := 16#84d2#;
   GL_TEXTURE19 : constant := 16#84d3#;
   GL_TEXTURE20 : constant := 16#84d4#;
   GL_TEXTURE21 : constant := 16#84d5#;
   GL_TEXTURE22 : constant := 16#84d6#;
   GL_TEXTURE23 : constant := 16#84d7#;
   GL_TEXTURE24 : constant := 16#84d8#;
   GL_TEXTURE25 : constant := 16#84d9#;
   GL_TEXTURE26 : constant := 16#84da#;
   GL_TEXTURE27 : constant := 16#84db#;
   GL_TEXTURE28 : constant := 16#84dc#;
   GL_TEXTURE29 : constant := 16#84dd#;
   GL_TEXTURE30 : constant := 16#84de#;
   GL_TEXTURE31 : constant := 16#84df#;

   GL_ACTIVE_TEXTURE : constant := 16#84e0#;
   GL_REPEAT         : constant := 16#2901#;
   GL_CLAMP_TO_EDGE  : constant := 16#812f#;



   -------------
   --  Functions
   --
   procedure glActiveTexture (texture  : in     GLenum);
   procedure glBindTexture   (target   : in     GLenum;
                              texture  : in     GLuint);
   procedure glBlendFunc     (sfactor  : in     GLenum;
                              dfactor  : in     GLenum);
   procedure glClear         (mask     : in     GLbitfield);
   procedure glClearColor    (red      : in     GLclampf;
                              green    : in     GLclampf;
                              blue     : in     GLclampf;
                              alpha    : in     GLclampf);
   procedure glClearDepthf   (depth    : in     GLclampf);
   procedure glClearStencil  (s        : in     GLint);
   procedure glColorMask     (red      : in     GLboolean;
                              green    : in     GLboolean;
                              blue     : in     GLboolean;
                              alpha    : in     GLboolean);
   procedure glCullFace      (mode     : in     GLenum);
   procedure glDepthFunc     (func     : in     GLenum);
   procedure glDepthMask     (flag     : in     GLboolean);
   procedure glDepthRangef   (zNear    : in     GLclampf;
                              zFar     : in     GLclampf);
   procedure glDisable       (cap      : in     GLenum);
   procedure glDrawArrays    (mode     : in     GLenum;
                              first    : in     GLint;
                              count    : in     GLsizei);
   procedure glDrawElements  (mode     : in     GLenum;
                              count    : in     GLsizei;
                              the_type : in     GLenum;
                              indices  : access GLvoid);
   procedure glEnable        (cap      : in     GLenum);
   procedure glFinish;
   procedure glFlush;
   procedure glFrontFace     (mode     : in     GLenum);
   procedure glGenTextures   (n        : in     GLsizei;
                              textures : access GLuint);
   function  glGetError                                  return GLenum;
   procedure glGetBooleanv   (pname    : in     GLenum;
                              params   : access GLboolean);
   procedure glGetFloatv     (pname    : in     GLenum;
                              params   : access GLfloat);
   procedure glGetIntegerv   (pname    : in     GLenum;
                              params   : access GLint);
   function  glGetString     (name     : in     GLenum)  return access GLubyte;
   procedure glGetTexParameteriv
                             (target   : in     GLenum;
                              pname    : in     GLenum;
                              params   : access GLint);
   procedure glHint          (target   : in     GLenum;
                              mode     : in     GLenum);
   function  glIsEnabled     (cap      : in     GLenum)   return GLboolean;
   procedure glLineWidth     (width    : in     GLfloat);
   procedure glPixelStorei   (pname    : in     GLenum;
                              param    : in     GLint);
   procedure glPolygonOffset (factor   : in     GLfloat;
                              units    : in     GLfloat);
   procedure glReadPixels    (x        : in     GLint;
                              y        : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei;
                              format   : in     GLenum;
                              the_type : in     GLenum;
                              pixels   : access GLvoid);
   procedure glScissor       (x        : in     GLint;
                              y        : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei);
   procedure glStencilFunc   (func     : in     GLenum;
                              ref      : in     GLint;
                              mask     : in     GLuint);
   procedure glStencilMask   (mask     : in     GLuint);
   procedure glStencilOp     (fail     : in     GLenum;
                              zfail    : in     GLenum;
                              zpass    : in     GLenum);
   procedure glTexImage2D    (target   : in     GLenum;
                              level    : in     GLint;
                              internalformat
                                       : in     GLenum;
                              width    : in     GLsizei;
                              height   : in     GLsizei;
                              border   : in     GLint;
                              format   : in     GLenum;
                              the_type : in     GLenum;
                              pixels   : access GLvoid);
   procedure glTexSubImage2D (target   : in     GLenum;
                              level    : in     GLint;
                              xoffset  : in     GLint;
                              yoffset  : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei;
                              format   : in     GLenum;
                              the_type : in     GLenum;
                              pixels   : access GLvoid);
   procedure glTexParameteri (target   : in     GLenum;
                              pname    : in     GLenum;
                              param    : in     GLint);
   procedure glViewport      (x        : in     GLint;
                              y        : in     GLint;
                              width    : in     GLsizei;
                              height   : in     GLsizei);



private

   pragma Import (StdCall, glActiveTexture, "glActiveTexture");
   pragma Import (Stdcall, glBindTexture,   "glBindTexture");
   pragma Import (Stdcall, glBlendFunc,     "glBlendFunc");
   pragma Import (Stdcall, glClear,         "glClear");
   pragma Import (Stdcall, glClearColor,    "glClearColor");
   pragma Import (Stdcall, glClearDepthf,   "glClearDepthf");
   pragma Import (Stdcall, glClearStencil,  "glClearStencil");
   pragma Import (Stdcall, glColorMask,     "glColorMask");
   pragma Import (Stdcall, glCullFace,      "glCullFace");
   pragma Import (Stdcall, glDepthFunc,     "glDepthFunc");
   pragma Import (Stdcall, glDepthMask,     "glDepthMask");
   pragma Import (Stdcall, glDepthRangef,   "glDepthRangef");
   pragma Import (Stdcall, glDisable,       "glDisable");
   pragma Import (Stdcall, glDrawArrays,    "glDrawArrays");
   pragma Import (Stdcall, glDrawElements,  "glDrawElements");
   pragma Import (Stdcall, glEnable,        "glEnable");
   pragma Import (Stdcall, glFinish,        "glFinish");
   pragma Import (Stdcall, glFlush,         "glFlush");
   pragma Import (Stdcall, glFrontFace,     "glFrontFace");
   pragma Import (Stdcall, glGenTextures,   "glGenTextures");
   pragma Import (Stdcall, glGetError,      "glGetError");
   pragma Import (StdCall, glGetBooleanv,   "glGetBooleanv");
   pragma Import (StdCall, glGetFloatv,     "glGetFloatv");
   pragma Import (StdCall, glGetIntegerv,   "glGetIntegerv");
   pragma Import (StdCall, glGetString,     "glGetString");
   pragma Import (StdCall, glGetTexParameteriv,
                                            "glGetTexParameteriv");
   pragma Import (Stdcall, glHint,          "glHint");
   pragma Import (Stdcall, glIsEnabled,     "glIsEnabled");
   pragma Import (Stdcall, glLineWidth,     "glLineWidth");
   pragma Import (Stdcall, glPixelStorei,   "glPixelStorei");
   pragma Import (Stdcall, glPolygonOffset, "glPolygonOffset");
   pragma Import (StdCall, glReadPixels,    "glReadPixels");
   pragma Import (Stdcall, glScissor,       "glScissor");
   pragma Import (Stdcall, glStencilFunc,   "glStencilFunc");
   pragma Import (Stdcall, glStencilMask,   "glStencilMask");
   pragma Import (Stdcall, glStencilOp,     "glStencilOp");
   pragma Import (StdCall, glTexImage2D,    "glTexImage2D");
   pragma Import (StdCall, glTexSubImage2D, "glTexSubImage2D");
   pragma Import (Stdcall, glTexParameteri, "glTexParameteri");
   pragma Import (Stdcall, glViewport,      "glViewport");

end GL;
