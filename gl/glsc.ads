-------------------------------------------------------------------------------
--  Filename       : glsc.ads
--  Date           : 2012-04-11
--  File Revision  : 6
--  Copyright      : Copyright (c) 2011, Eutopia Labs Ltd,
--                 : All rights reserved.
--  Purpose        : Raw Ada Binding for Graphics Library API
--  Programmer     : David Rees
-------------------------------------------------------------------------------
--  Specification  : Safety Critical Profile Version 1.0.1
--  Overview       : http://www.khronos.org/openglsc
--  Reference      : http://www.khronos.org/registry/glsc/specs/sc_spec_1_0_1.pdf
-------------------------------------------------------------------------------
--  Certifications : NONE
--  Compliances    : NONE
--  Guidance       : ISO/IEC TR 15942:2000, Ada 2005 LRM,
--                 : Ada Quality and Style Guide [AQS]
--  Status         : Work In Progress
-------------------------------------------------------------------------------
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that:
--
--   1. Redistributions of this source code must retain the copyright notice,
--      this list of conditions, and the following disclaimer.
--   2. Except in the copyright notice above, the names of its contributors
--      and copyright holders shall not be used to endorse or otherwise
--      promote the sale of, use of, or any other dealings in; this software
--      without prior written authorization from the copyright holder.
--   3. Derivatives or redistributions of this work shall not be used in any
--      form by individuals, entities, or groups who deny individuals of their
--      inalienable natural human rights, or who violate any articles of the
--      Universal Declaration of Human Rights.
--   4. This licence is subject to change during development, at the sole
--      discretion of the copyright holder.
--
-- THIS SOFTWARE IS PROVIDED BY THE  COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY  EXPRESS OR IMPLIED WARRANTIES,  INCLUDING, BUT NOT LIMITED  TO, THE
-- IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS FOR  A PARTICULAR PURPOSE
-- ARE  DISCLAIMED. IN  NO EVENT  SHALL  THE COPYRIGHT  HOLDER OR  CONTRIBUTORS
-- BE  LIABLE FOR  ANY  DIRECT, INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY,  OR
-- CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT  LIMITED  TO,  PROCUREMENT  OF
-- SUBSTITUTE GOODS  OR SERVICES; LOSS  OF USE,  DATA, OR PROFITS;  OR BUSINESS
-- INTERRUPTION)  HOWEVER CAUSED  AND ON  ANY THEORY  OF LIABILITY,  WHETHER IN
-- CONTRACT,  STRICT LIABILITY,  OR  TORT (INCLUDING  NEGLIGENCE OR  OTHERWISE)
-- ARISING IN ANY WAY  OUT OF THE USE OF THIS SOFTWARE, EVEN  IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

with Interfaces, System;

package GLSC is

   pragma Pure; -- No state at runtime, so it can be consistently replicated in
                -- more than one partition.


   ----------------------------------------------------------------------------
   -- External GL Library Constants -------------------------------------------
   ----------------------------------------------------------------------------

   -- Extensions
   GL_OSC_VERSION_1_0      : constant := 1;
   GL_EXT_paletted_texture : constant := 1; -- required extension
   GL_OES_single_precision : constant := 1; -- core addition

   -- ClearBufferMask
   GL_DEPTH_BUFFER_BIT   : constant := 16#00000100#;
   GL_STENCIL_BUFFER_BIT : constant := 16#00000400#;
   GL_COLOR_BUFFER_BIT   : constant := 16#00004000#;

   -- Boolean
   GL_FALSE : constant := 16#0000#;
   GL_TRUE  : constant := 16#0001#;

   -- BeginMode
   GL_POINTS         : constant := 16#0000#;
   GL_LINES          : constant := 16#0001#;
   GL_LINE_LOOP      : constant := 16#0002#;
   GL_LINE_STRIP     : constant := 16#0003#;
   GL_TRIANGLES      : constant := 16#0004#;
   GL_TRIANGLE_STRIP : constant := 16#0005#;
   GL_TRIANGLE_FAN   : constant := 16#0006#;

   -- AlphaFunction
   -- GL_LEQUAL : constant := 16#0203#;
   -- GL_ALWAYS : constant := 16#0207#;

   -- BlendingFactorDest
   GL_ZERO                : constant := 0;
   GL_ONE                 : constant := 1;
   GL_ONE_MINUS_SRC_ALPHA : constant := 16#0303#;

   -- BlendingFactorSrc
   -- GL_ONE             : constant := 1;
   GL_SRC_ALPHA_SATURATE : constant := 16#0308#;
   GL_SRC_ALPHA          : constant := 16#0302#;

   -- ColorMaterialFace
   -- GL_FRONT_AND_BACK : constant := 16#0408#;

   -- ColorMaterialParameter
   -- GL_AMBIENT_AND_DIFFUSE : constant := 16#1602#;

   -- ColorPointerType
   -- GL_FLOAT         : constant := 16#1406#;
   -- GL_UNSIGNED_BYTE : constant := 16#1401#;

   -- CullFaceMode
   GL_FRONT          : constant := 16#0404#;
   GL_BACK           : constant := 16#0405#;
   GL_FRONT_AND_BACK : constant := 16#0408#;

   -- DepthFunction
   -- GL_LESS   : constant := 16#0201#;
   -- GL_LEQUAL : constant := 16#0203#;
   -- GL_ALWAYS : constant := 16#0207#;

   -- EnableCap
   GL_LIGHTING            : constant := 16#0B50#;
   GL_TEXTURE_2D          : constant := 16#0DE1#;
   GL_CULL_FACE           : constant := 16#0B44#;
   GL_ALPHA_TEST          : constant := 16#0BC0#;
   GL_BLEND               : constant := 16#0BE2#;
   GL_STENCIL_TEST        : constant := 16#0B90#;
   GL_DEPTH_TEST          : constant := 16#0B71#;
   GL_LIGHT0              : constant := 16#4000#;
   GL_LIGHT1              : constant := 16#4001#;
   GL_POINT_SMOOTH        : constant := 16#0B10#;
   GL_LINE_STIPPLE        : constant := 16#0B24#;
   GL_LINE_SMOOTH         : constant := 16#0B20#;
   GL_SCISSOR_TEST        : constant := 16#0C11#;
   GL_COLOR_MATERIAL      : constant := 16#0B57#;
   GL_NORMALIZE           : constant := 16#0BA1#;
   GL_RESCALE_NORMAL      : constant := 16#803A#;
   GL_POLYGON_OFFSET_FILL : constant := 16#8037#;
   GL_POLYGON_STIPPLE     : constant := 16#0B42#;
   GL_VERTEX_ARRAY        : constant := 16#8074#;
   GL_NORMAL_ARRAY        : constant := 16#8075#;
   GL_COLOR_ARRAY         : constant := 16#8076#;
   GL_TEXTURE_COORD_ARRAY : constant := 16#8078#;

   -- ErrorCode
   GL_NO_ERROR          : constant := 0;
   GL_INVALID_ENUM      : constant := 16#0500#;
   GL_INVALID_VALUE     : constant := 16#0501#;
   GL_INVALID_OPERATION : constant := 16#0502#;
   GL_STACK_OVERFLOW    : constant := 16#0503#;
   GL_STACK_UNDERFLOW   : constant := 16#0504#;
   GL_OUT_OF_MEMORY     : constant := 16#0505#;

   -- FrontFaceDirection
   GL_CW  : constant := 16#0900#;
   GL_CCW : constant := 16#0901#;

   -- GetBooleanv
   GL_DEPTH_WRITEMASK : constant := 16#0B72#;
   GL_COLOR_WRITEMASK : constant := 16#0C23#;

   -- GetFloatv
   GL_CURRENT_COLOR                 : constant := 16#0B00#;
   GL_CURRENT_NORMAL                : constant := 16#0B02#;
   GL_CURRENT_TEXTURE_COORDS        : constant := 16#0B03#;
   GL_CURRENT_RASTER_COLOR          : constant := 16#0B04#;
   GL_CURRENT_RASTER_TEXTURE_COORDS : constant := 16#0B06#;
   GL_POINT_SIZE                    : constant := 16#0B11#;
   GL_SMOOTH_POINT_SIZE_RANGE       : constant := 16#0B12#;
   GL_SMOOTH_POINT_SIZE_GRANULARITY : constant := 16#0B13#;
   GL_LINE_WIDTH                    : constant := 16#0B21#;
   GL_SMOOTH_LINE_WIDTH_RANGE       : constant := 16#0B22#;
   GL_SMOOTH_LINE_WIDTH_GRANULARITY : constant := 16#0B23#;

   -- GetIntegerv
   GL_DEPTH_RANGE              : constant := 16#0B70#;
   GL_DEPTH_CLEAR_VALUE        : constant := 16#0B73#;
   GL_ALPHA_TEST_REF           : constant := 16#0BC2#;
   GL_COLOR_CLEAR_VALUE        : constant := 16#0C22#;
   GL_POLYGON_OFFSET_UNITS     : constant := 16#2A00#;
   GL_POLYGON_OFFSET_FACTOR    : constant := 16#8038#;
   GL_ALIASED_POINT_SIZE_RANGE : constant := 16#846D#;
   GL_ALIASED_LINE_WIDTH_RANGE : constant := 16#846E#;

   GL_MATRIX_MODE             : constant := 16#0BA0#;
   GL_VIEWPORT                : constant := 16#0BA2#;
   GL_MODELVIEW_STACK_DEPTH   : constant := 16#0BA3#;
   GL_PROJECTION_STACK_DEPTH  : constant := 16#0BA4#;
   GL_MODELVIEW_MATRIX        : constant := 16#0BA6#;
   GL_PROJECTION_MATRIX       : constant := 16#0BA7#;
   GL_LINE_STIPPLE_PATTERN    : constant := 16#0B25#;
   GL_LINE_STIPPLE_REPEAT     : constant := 16#0B26#;
   GL_MAX_LIST_NESTING        : constant := 16#0B31#;
   GL_LIST_BASE               : constant := 16#0B32#;
   GL_CULL_FACE_MODE          : constant := 16#0B45#;
   GL_FRONT_FACE              : constant := 16#0B46#;
   GL_DEPTH_FUNC              : constant := 16#0B74#;
   GL_STENCIL_CLEAR_VALUE     : constant := 16#0B91#;
   GL_STENCIL_FUNC            : constant := 16#0B92#;
   GL_STENCIL_VALUE_MASK      : constant := 16#0B93#;
   GL_STENCIL_FAIL            : constant := 16#0B94#;
   GL_STENCIL_PASS_DEPTH_FAIL : constant := 16#0B95#;
   GL_STENCIL_PASS_DEPTH_PASS : constant := 16#0B96#;
   GL_STENCIL_REF             : constant := 16#0B97#;
   GL_STENCIL_WRITEMASK       : constant := 16#0B98#;
   GL_ALPHA_TEST_FUNC         : constant := 16#0BC1#;
   GL_BLEND_DST               : constant := 16#0BE0#;
   GL_BLEND_SRC               : constant := 16#0BE1#;
   GL_SCISSOR_BOX             : constant := 16#0C10#;

   GL_POLYGON_SMOOTH_HINT     : constant := 16#0C53#;

   GL_MAX_LIGHTS                 : constant := 16#0D31#;
   GL_MAX_TEXTURE_SIZE           : constant := 16#0D33#;
   GL_MAX_MODELVIEW_STACK_DEPTH  : constant := 16#0D36#;
   GL_MAX_PROJECTION_STACK_DEPTH : constant := 16#0D38#;
   GL_MAX_VIEWPORT_DIMS          : constant := 16#0D3A#;
   GL_SUBPIXEL_BITS              : constant := 16#0D50#;
   GL_RED_BITS                   : constant := 16#0D52#;
   GL_GREEN_BITS                 : constant := 16#0D53#;
   GL_BLUE_BITS                  : constant := 16#0D54#;
   GL_ALPHA_BITS                 : constant := 16#0D55#;
   GL_DEPTH_BITS                 : constant := 16#0D56#;
   GL_STENCIL_BITS               : constant := 16#0D57#;
   GL_VERTEX_ARRAY_SIZE          : constant := 16#807A#;
   GL_VERTEX_ARRAY_TYPE          : constant := 16#807B#;
   GL_VERTEX_ARRAY_STRIDE        : constant := 16#807C#;
   GL_NORMAL_ARRAY_TYPE          : constant := 16#807E#;
   GL_NORMAL_ARRAY_STRIDE        : constant := 16#807F#;
   GL_COLOR_ARRAY_SIZE           : constant := 16#8081#;
   GL_COLOR_ARRAY_TYPE           : constant := 16#8082#;
   GL_COLOR_ARRAY_STRIDE         : constant := 16#8083#;
   GL_TEXTURE_COORD_ARRAY_SIZE   : constant := 16#8088#;
   GL_TEXTURE_COORD_ARRAY_TYPE   : constant := 16#8089#;
   GL_TEXTURE_COORD_ARRAY_STRIDE : constant := 16#808A#;
   GL_SHADE_MODEL                : constant := 16#0B54#;
   GL_TEXTURE_BINDING_2D         : constant := 16#8069#;
   GL_MAX_ELEMENTS_VERTICES      : constant := 16#80E8#;
   GL_MAX_ELEMENTS_INDICES       : constant := 16#80E9#;
   GL_ACTIVE_TEXTURE             : constant := 16#84E0#;
   GL_CLIENT_ACTIVE_TEXTURE      : constant := 16#84E1#;
   GL_MAX_TEXTURE_UNITS          : constant := 16#84E2#;

   -- GetMaterialfv
   -- GL_AMBIENT   : constant := 16#1200#;
   -- GL_DIFFUSE   : constant := 16#1201#;
   -- GL_SPECULAR  : constant := 16#1202#;
   -- GL_EMISSION  : constant := 16#1600#;
   -- GL_SHININESS : constant := 16#1601#;

   -- GetLightfv
   -- GL_AMBIENT  : constant := 16#1200#;
   -- GL_DIFFUSE  : constant := 16#1201#;
   -- GL_SPECULAR : constant := 16#1202#;
   -- GL_POSITION : constant := 16#1203#;

   -- GetPointerv
   GL_VERTEX_ARRAY_POINTER        : constant := 16#808E#;
   GL_NORMAL_ARRAY_POINTER        : constant := 16#808F#;
   GL_COLOR_ARRAY_POINTER         : constant := 16#8090#;
   GL_TEXTURE_COORD_ARRAY_POINTER : constant := 16#8092#;

   -- GetTexParameter
   -- GL_TEXTURE_MAG_FILTER : constant := 16#28002#;
   -- GL_TEXTURE_MIN_FILTER : constant := 16#28012#;
   -- GL_TEXTURE_WRAP_S     : constant := 16#28022#;
   -- GL_TEXTURE_WRAP_T     : constant := 16#28032#;

   -- GetTexEnvfv
   -- GL_TEXTURE_ENV_MODE  : constant := 16#22002#;
   -- GL_TEXTURE_ENV_COLOR : constant := 16#22012#;

   -- HintMode
   GL_DONT_CARE : constant := 16#1100#;
   GL_FASTEST   : constant := 16#1101#;
   GL_NICEST    : constant := 16#1102#;

   -- HintTarget
   GL_PERSPECTIVE_CORRECTION_HINT : constant := 16#0C50#;
   GL_POINT_SMOOTH_HINT           : constant := 16#0C51#;
   GL_LINE_SMOOTH_HINT            : constant := 16#0C52#;

   -- IsEnabled
   -- GL_LIGHTING            : constant := 16#0B50#;
   -- GL_TEXTURE_2D          : constant := 16#0DE1#;
   -- GL_CULL_FACE           : constant := 16#0B44#;
   -- GL_ALPHA_TEST          : constant := 16#0BC0#;
   -- GL_BLEND               : constant := 16#0BE2#;
   -- GL_STENCIL_TEST        : constant := 16#0B90#;
   -- GL_DEPTH_TEST          : constant := 16#0B71#;
   -- GL_LIGHT0              : constant := 16#4000#;
   -- GL_LIGHT1              : constant := 16#4001#;
   -- GL_POINT_SMOOTH        : constant := 16#0B10#;
   -- GL_LINE_STIPPLE        : constant := 16#0B24#;
   -- GL_LINE_SMOOTH         : constant := 16#0B20#;
   -- GL_SCISSOR_TEST        : constant := 16#0C11#;
   -- GL_COLOR_MATERIAL      : constant := 16#0B57#;
   -- GL_NORMALIZE           : constant := 16#0BA1#;
   -- GL_RESCALE_NORMAL      : constant := 16#803A#;
   -- GL_POLYGON_OFFSET_FILL : constant := 16#8037#;
   -- GL_POLYGON_STIPPLE     : constant := 16#0B42#;
   -- GL_VERTEX_ARRAY        : constant := 16#8074#;
   -- GL_NORMAL_ARRAY        : constant := 16#8075#;
   -- GL_COLOR_ARRAY         : constant := 16#8076#;
   -- GL_TEXTURE_COORD_ARRAY : constant := 16#8078#;

   -- LightModelParameter
   GL_LIGHT_MODEL_AMBIENT : constant := 16#0B53#;

   -- LightParameter
   GL_AMBIENT  : constant := 16#1200#;
   GL_DIFFUSE  : constant := 16#1201#;
   GL_SPECULAR : constant := 16#1202#;
   GL_POSITION : constant := 16#1203#;

   -- ListMode
   GL_COMPILE                : constant := 16#1300#;
   -- ListMode
   -- GL_COMPILE_AND_EXECUTE : constant := 16#1301#;

   -- DataType
   GL_BYTE              : constant := 16#1400#;
   GL_UNSIGNED_BYTE     : constant := 16#1401#;
   -- GL_SHORT          : constant := 16#1402#;
   -- GL_UNSIGNED_SHORT : constant := 16#1403#;
   GL_INT               : constant := 16#1404#;
   GL_UNSIGNED_INT      : constant := 16#1405#;
   GL_FLOAT             : constant := 16#1406#;

   -- LogicOp

   -- MaterialFace
   -- GL_FRONT_AND_BACK : constant := 16#0408#;

   -- MaterialParameter
   -- GL_AMBIENT          : constant := 16#1200#;
   -- GL_DIFFUSE          : constant := 16#1201#;
   -- GL_SPECULAR         : constant := 16#1202#;
   GL_EMISSION            : constant := 16#1600#;
   GL_SHININESS           : constant := 16#1601#;
   GL_AMBIENT_AND_DIFFUSE : constant := 16#1602#;

   -- MatrixMode
   GL_MODELVIEW  : constant := 16#1700#;
   GL_PROJECTION : constant := 16#1701#;

   -- NormalPointerType
   -- GL_FLOAT : constant := 16#1406#;

   -- PixelFormat
   GL_ALPHA           : constant := 16#1906#;
   GL_RGB             : constant := 16#1907#;
   GL_RGBA            : constant := 16#1908#;
   GL_LUMINANCE       : constant := 16#1909#;
   GL_LUMINANCE_ALPHA : constant := 16#190A#;
   GL_COLOR_INDEX     : constant := 16#1900#;

   -- PixelStoreParameter
   GL_UNPACK_ALIGNMENT : constant := 16#0CF5#;
   GL_PACK_ALIGNMENT   : constant := 16#0D05#;

   -- PixelType
   -- GL_UNSIGNED_BYTE : constant := 16#1401#;

   -- ShadingModel
   GL_COLOR  : constant := 16#1800#;

   GL_FLAT   : constant := 16#1D00#;
   GL_SMOOTH : constant := 16#1D01#;

   -- StencilFunction
   GL_NEVER    : constant := 16#0200#;
   GL_LESS     : constant := 16#0201#;
   GL_EQUAL    : constant := 16#0202#;
   GL_LEQUAL   : constant := 16#0203#;
   GL_GREATER  : constant := 16#0204#;
   GL_NOTEQUAL : constant := 16#0205#;
   GL_GEQUAL   : constant := 16#0206#;
   GL_ALWAYS   : constant := 16#0207#;

   -- StencilOp
   -- GL_ZERO : constant := 0;
   GL_KEEP    : constant := 16#1E00#;
   GL_REPLACE : constant := 16#1E01#;
   GL_INCR    : constant := 16#1E02#;
   GL_DECR    : constant := 16#1E03#;
   GL_INVERT  : constant := 16#150A#;

   -- StringName
   GL_VENDOR     : constant := 16#1F00#;
   GL_RENDERER   : constant := 16#1F01#;
   GL_VERSION    : constant := 16#1F02#;
   GL_EXTENSIONS : constant := 16#1F03#;

   -- TexCoordPointerType
   -- GL_FLOAT : constant := 16#1406#;

   -- TextureEnvMode
   GL_MODULATE   : constant := 16#2100#;
   GL_DECAL      : constant := 16#2101#;
   -- GL_BLEND   : constant := 16#0BE2#;
   GL_ADD        : constant := 16#0104#;
   -- GL_REPLACE : constant := 16#1E01#;

   -- TextureEnvParameter
   GL_TEXTURE_ENV_MODE  : constant := 16#2200#;
   GL_TEXTURE_ENV_COLOR : constant := 16#2201#;

   -- TextureEnvTarget
   GL_TEXTURE_ENV : constant := 16#2300#;

   -- TextureMagFilter
   GL_NEAREST : constant := 16#2600#;
   GL_LINEAR  : constant := 16#2601#;

   -- TextureMinFilter
   -- GL_NEAREST             : constant := 16#2600#;
   -- GL_LINEAR              : constant := 16#2601#;
   GL_NEAREST_MIPMAP_NEAREST : constant := 16#2700#;
   GL_LINEAR_MIPMAP_NEAREST  : constant := 16#2701#;
   GL_NEAREST_MIPMAP_LINEAR  : constant := 16#2702#;
   GL_LINEAR_MIPMAP_LINEAR   : constant := 16#2703#;

   -- TextureParameterName
   GL_TEXTURE_MAG_FILTER : constant := 16#2800#;
   GL_TEXTURE_MIN_FILTER : constant := 16#2801#;
   GL_TEXTURE_WRAP_S     : constant := 16#2802#;
   GL_TEXTURE_WRAP_T     : constant := 16#2803#;

   -- TextureTarget
   -- GL_TEXTURE_2D : constant := 16#0DE1#;

   -- TextureUnit
   GL_TEXTURE0  : constant := 16#84C0#;
   GL_TEXTURE1  : constant := 16#84C1#;
   GL_TEXTURE2  : constant := 16#84C2#;
   GL_TEXTURE3  : constant := 16#84C3#;
   GL_TEXTURE4  : constant := 16#84C4#;
   GL_TEXTURE5  : constant := 16#84C5#;
   GL_TEXTURE6  : constant := 16#84C6#;
   GL_TEXTURE7  : constant := 16#84C7#;
   GL_TEXTURE8  : constant := 16#84C8#;
   GL_TEXTURE9  : constant := 16#84C9#;
   GL_TEXTURE10 : constant := 16#84CA#;
   GL_TEXTURE11 : constant := 16#84CB#;
   GL_TEXTURE12 : constant := 16#84CC#;
   GL_TEXTURE13 : constant := 16#84CD#;
   GL_TEXTURE14 : constant := 16#84CE#;
   GL_TEXTURE15 : constant := 16#84CF#;
   GL_TEXTURE16 : constant := 16#84D0#;
   GL_TEXTURE17 : constant := 16#84D1#;
   GL_TEXTURE18 : constant := 16#84D2#;
   GL_TEXTURE19 : constant := 16#84D3#;
   GL_TEXTURE20 : constant := 16#84D4#;
   GL_TEXTURE21 : constant := 16#84D5#;
   GL_TEXTURE22 : constant := 16#84D6#;
   GL_TEXTURE23 : constant := 16#84D7#;
   GL_TEXTURE24 : constant := 16#84D8#;
   GL_TEXTURE25 : constant := 16#84D9#;
   GL_TEXTURE26 : constant := 16#84DA#;
   GL_TEXTURE27 : constant := 16#84DB#;
   GL_TEXTURE28 : constant := 16#84DC#;
   GL_TEXTURE29 : constant := 16#84DD#;
   GL_TEXTURE30 : constant := 16#84DE#;
   GL_TEXTURE31 : constant := 16#84DF#;

   -- TextureWrapMode
   GL_REPEAT        : constant := 16#2901#;
   GL_CLAMP_TO_EDGE : constant := 16#812F#;

   -- PixelInternalFormat
   GL_COLOR_INDEX8_EXT : constant := 16#80E5#;

   -- VertexPointerType
   -- GL_FLOAT : constant := 16#1406#;

   -- Paletted Textures Extension
   GL_COLOR_TABLE_FORMAT_EXT         : constant := 16#80D8#;
   GL_COLOR_TABLE_WIDTH_EXT          : constant := 16#80D9#;
   GL_COLOR_TABLE_RED_SIZE_EXT       : constant := 16#80DA#;
   GL_COLOR_TABLE_GREEN_SIZE_EXT     : constant := 16#80DB#;
   GL_COLOR_TABLE_BLUE_SIZE_EXT      : constant := 16#80DC#;
   GL_COLOR_TABLE_ALPHA_SIZE_EXT     : constant := 16#80DD#;
   GL_COLOR_TABLE_LUMINANCE_SIZE_EXT : constant := 16#80DE#;
   GL_COLOR_TABLE_INTENSITY_SIZE_EXT : constant := 16#80DF#;


   -- C related Constants -----------------------------------------------------
   -- Only some of the type specifications from Interfaces.C are used, so for
   -- simplicity's sake, only the ones referenced will be defined in this spec.
   ----------------------------------------------------------------------------
   CHAR_BIT  : constant := 8;

   -- 2's complement implementations have SCHAR_MIN equal to –128
   SCHAR_MIN : constant := -128; -- Minimum value for a signed char
   SCHAR_MAX : constant := 127;  -- Maximum value for a signed char
   UCHAR_MAX : constant := 255;  -- Maximum value for an unsigned char

   -----------------------------------------------------------------------------
   -- GL types - Note: GL types are NOT C types. -------------------------------
   -----------------------------------------------------------------------------
   -- Double-precision floating-point (parameters and data types) are
   -- eliminated completely in the Safety-Critical GL specification.
   -- All integer quantity parameters (widths, heights, array lengths, etc.)
   -- should be typed GLsizei, not GLint or GLuint.  Note that GLsizei
   -- generates an error only for negative values, so all integer quantity
   -- parameters should accept the value zero.
   -- Representation Clause's specify the minimum bit widths as stated
   -- by the API documentation. The only exception is Boolean, which the
   -- documentation states a MBW of 1-bit, whereas Ada's Boolean bit width
   -- of 8-bits is prefered.

   type signed_char is range SCHAR_MIN .. SCHAR_MAX;
   for signed_char'Size use CHAR_BIT;	-- Minimum bit width for signed_char

   type unsigned is mod 2 ** Integer'Size;
   type unsigned_short is mod 2 ** Short_Integer'Size;

   type unsigned_char is mod (UCHAR_MAX + 1);
   for unsigned_char'Size use CHAR_BIT; -- Minimum bit width for unsigned_char

   type GLenum is new unsigned;         -- Enumerated Binary Integer
   for GLenum'Size use 32;              -- Minimum bit width for GLenum

   type GLboolean is new unsigned_char; -- Boolean
   for GLBoolean'Size use 8;            -- Minimum bit width for GLBoolean

   type GLbitfield is new unsigned;     -- Bit Field
   for GLBitfield'Size use 32;          -- Minimum bit width for GLBitfield

   type GLbyte is new signed_char;      -- Signed 2's Complement Binary Integer
   for GLbyte'Size use 8;               -- Minimum bit width for GLByte

   type GLint is new Integer;           -- Signed 2's Complement Binary Integer
   for GLint'Size use 32;               -- Minimum bit width for GLint

   type GLsizei is new Integer;         -- Non-negative binary integer
   for GLsizei'Size use 32;             -- Minimum bit width for GLsizei

   type GLubyte is new unsigned_char;   -- Unsigned Binary Integer
   for GLubyte'Size use 8;              -- Minimum bit width for GLubyte

   type GLshort is new Short_Integer;   -- Signed 2's Complement Binary Integer
   for GLshort'Size use 16;             -- Minimum bit width for GLshort

   type GLushort is new unsigned_short; -- Unsigned Binary Integer
   for GLushort'Size use 16;            -- Minimum bit width for GLushort

   type GLuint is new unsigned;         -- Unsigned Binary Integer
   for GLuint'Size use 32;              -- Minimum bit width for GLuint

   ----------------------------------------------------------------------------
   -- Float Types -------------------------------------------------------------
   -- If a floating point subtype is unconstrained, then assignments to variables
   -- of the subtype involve only Overflow_Checks, never Range_Checks.
   -- All floating point parameters that are clamped to the range [0,1] when
   -- received, should be typed GLclampf, not GLfloat.
   ----------------------------------------------------------------------------
   type GLFloat is new float;           -- Single-Precision Floating-Point
   for GLFloat'Size use 32;             -- Minimum bit width for GLFloat

   ----------------------------------------------------------------------------
   -- GLclampf raises Constraint_Error if value is not within the range [0,1]
   -- WARNING: Check that your implementation really does 'clamp' values.
   -- Floating-point clamped to [0,1] (minimum bit width 32 bits) with 5
   -- decimal digits of precision available for floating-point types
   ----------------------------------------------------------------------------
   type GLclampf is new float range 0.0 .. 1.0;
   for GLclampf'Size use 32;            -- Minimum bit width for GLclampf

   subtype GLvoid is System.Address;

   -- Internal convenience
   type u_GLfuncptr is access procedure;

   ----------------------------------------------------------------------------
   -- External Library API procedures -----------------------------------------
   ----------------------------------------------------------------------------

   procedure glActiveTexture ( Texture : in GLenum );

   procedure glAlphaFunc (  Func : in GLenum;
                            Ref  : in GLclampf );

   procedure glBegin ( Mode : in GLenum );

   procedure glBindTexture (  Target  : in GLenum;
                              Texture : in GLuint );

   procedure glBitmap ( Width  : in GLsizei;
                        Height : in GLsizei;
                        Xorig  : in GLfloat;
                        Yorig  : in GLfloat;
                        Xmove  : in GLfloat;
                        Ymove  : in GLfloat;
                        Bitmap : access GLubyte );

   procedure glBlendFunc ( Sfactor : in GLenum;
                           Dfactor : in GLenum );

   procedure glCallLists ( N      : in GLsizei;
                           C_Type : in GLenum;
                           Lists  : in System.Address );

   procedure glClear ( Mask : in GLbitfield );

   procedure glClearColor ( Red   : in GLclampf;
                            Green : in GLclampf;
                            Blue  : in GLclampf;
                            Alpha : in GLclampf );

   procedure glClearDepthf ( Depth : in GLclampf );

   procedure glClearStencil ( S : in GLint);

   procedure glClientActiveTexture ( Texture : in GLenum );

   procedure glColor4f ( Red   : in GLfloat;
                         Green : in GLfloat;
                         Blue  : in GLfloat;
                         Alpha : in GLfloat );

   procedure glColor4fv ( V : access GLfloat );

   procedure glColor4ub ( Red   : in GLubyte;
                          Green : in GLubyte;
                          Blue  : in GLubyte;
                          Alpha : in GLubyte );

   procedure glColorMask ( Red   : in GLboolean;
                           Green : in GLboolean;
                           Blue  : in GLboolean;
                           Alpha : in GLboolean );

   procedure glColorPointer ( Size    : in GLint;
                              C_Type  : in GLenum;
                              Stride  : in GLsizei;
                              Pointer : in System.Address );

   procedure glColorSubTableEXT ( Target : in GLenum;
                                  Start  : in GLsizei;
                                  Count  : in GLsizei;
                                  Format : in GLenum;
                                  C_Type : in GLenum;
                                  Table  : in System.Address );

   procedure glColorTableEXT ( Target         : in GLenum;
                               Internalformat : in GLenum;
                               Width          : in GLsizei;
                               Format         : in GLenum;
                               C_Type         : in GLenum;
                               Table          : in System.Address );

   procedure glCopyPixels ( X      : in GLint;
                            Y      : in GLint;
                            Width  : in GLsizei;
                            Height : in GLsizei;
                            C_Type : in GLenum );

   procedure glCullFace ( Mode : in GLenum );

   procedure glDepthFunc ( Func : in GLenum );

   procedure glDepthMask ( Flag : in GLboolean );

   procedure glDepthRangef (  zNear : in GLclampf;
                              zFar  : in GLclampf );

   procedure glDisable ( Cap : in GLenum );

   procedure glDisableClientState ( C_Array : in GLenum );

   procedure glDrawArrays ( Mode  : in GLenum;
                            First : in GLint;
                            Count : in GLsizei );

   procedure glDrawElements ( Mode    : in GLenum;
                              Count   : in GLsizei;
                              C_Type  : in GLenum;
                              Indices : in System.Address );

   procedure glDrawPixels ( Width  : in GLsizei;
                            Height : in GLsizei;
                            Format : in GLenum;
                            C_Type : in GLenum;
                            Pixels : in System.Address );

   procedure glEnable ( Cap : in GLenum );

   procedure glEnableClientState ( C_Array : in GLenum );

   procedure glEnd;

   procedure glEndList;

   procedure glFinish;

   procedure glFlush;

   procedure glFrontFace ( Mode : in GLenum );

   procedure glFrustumf ( Left   : in GLfloat;
                          Right  : in GLfloat;
                          Bottom : in GLfloat;
                          Top    : in GLfloat;
                          zNear  : in GLfloat;
                          zFar   : in GLfloat );

   function glGenLists ( C_Range : in GLsizei ) return GLuint;

   procedure glGenTextures ( N        : in GLsizei;
                             Textures : access GLuint );

   function glGetError return GLenum;

   procedure glGetBooleanv ( Pname  : in GLenum;
                             params : access GLboolean );

   procedure glGetColorTableEXT ( Target : in GLenum;
                                  Format : in GLenum;
                                  C_Type : in GLenum;
                                  Table  : in System.Address );

   procedure glGetColorTableParameterivEXT ( Target : in GLenum;
                                             Pname  : in GLenum;
                                             Params : access GLint );

   procedure glGetFloatv ( Pname  : in GLenum;
                           Params : access GLfloat );

   procedure glGetIntegerv ( Pname  : in GLenum;
                             Params : access GLint );

   procedure glGetLightfv ( Light  : GLenum;
                            Pname  : GLenum;
                            Params : access GLfloat );

   procedure glGetMaterialfv ( Face   : in GLenum;
                               Pname  : in GLenum;
                               Params : access GLfloat );


   procedure glGetPointerv ( Pname  : in GLenum;
                             Params : in System.Address );

   procedure glGetPolygonStipple ( Mask : access GLubyte );

   procedure glGetTexEnvfv ( Target : in GLenum;
                             Pname  : in GLenum;
                             Params : access GLfloat );

   procedure glGetTexEnviv ( Target : in GLenum;
                             Pname  : in GLenum;
                             Params : access GLint );

   procedure glGetTexParameteriv ( Target : in GLenum;
                                   Pname  : in GLenum;
                                   Params : access GLint );

   function glGetString ( Name : in GLenum ) return access GLubyte;

   procedure glHint ( Target : in GLenum;
                      Mode   : in GLenum );

   function glIsEnabled ( Cap : in GLenum ) return GLboolean;

   procedure glLightfv ( Light  : in GLenum;
                         Pname  : in GLenum;
                         Params : access GLfloat );

   procedure glLightModelfv ( Pname  : in GLenum;
                              Params : access GLfloat );

   procedure glLineStipple ( Factor  : in GLint;
                             Pattern : GLushort );

   procedure glLineWidth ( Width : in GLfloat );

   procedure glListBase ( Base : in GLuint );

   procedure glLoadIdentity;

   procedure glLoadMatrixf ( M : access GLfloat );

   procedure glMaterialf ( Face  : in GLenum;
                           Pname : in GLenum;
                           Param : in GLfloat );

   procedure glMaterialfv ( Face   : in GLenum;
                            Pname  : in GLenum;
                            Params : access GLfloat );

   procedure glMatrixMode ( Mode : in GLenum );

   procedure glMultMatrixf ( M : access GLfloat );

   procedure glMultiTexCoord2f ( Target : in GLenum;
                                 S      : in GLfloat;
                                 T      : in GLfloat );

   procedure glMultiTexCoord2fv ( Target : in GLenum;
                                  V      : access GLfloat );

   procedure glNewList ( List : in GLuint;
                         Mode : in GLenum );

   procedure glNormal3f ( NX : in GLfloat;
                          NY : in GLfloat;
                          NZ : in GLfloat );

   procedure glNormal3fv ( V : access GLfloat );

   procedure glNormalPointer ( C_Type  : in GLenum;
                               Stride  : in GLsizei;
                               Pointer : in System.Address );

   procedure glOrthof ( Left     : in GLfloat;
                        Right    : in GLfloat;
                        Bottom   : in GLfloat;
                        Top      : in GLfloat;
                        zNear    : in GLfloat;
                        zFar     : in GLfloat );

   procedure glPixelStorei ( Pname : in GLenum;
                             Param : in GLint );

   procedure glPointSize ( Size : in GLfloat );

   procedure glPolygonOffset ( Factor : in GLfloat;
                               Units  : in GLfloat );

   procedure glPolygonStipple ( Mask : access GLubyte );

   procedure glPopMatrix;

   procedure glPushMatrix;

   procedure glRasterPos3f ( X : in GLfloat;
                             Y : in GLfloat;
                             Z : in GLfloat );

   procedure glReadPixels ( X      : in GLint;
                            Y      : in GLint;
                            Width  : in GLsizei;
                            Height : in GLsizei;
                            Format : in GLenum;
                            C_Type : in GLenum;
                            Pixels : in System.Address );

   procedure glRotatef ( Angle : in GLfloat;
                         X     : in GLfloat;
                         Y     : in GLfloat;
                         Z     : in GLfloat );

   procedure glScalef ( X : in GLfloat;
                        Y : in GLfloat;
                        Z : in GLfloat );

   procedure glScissor ( X      : in GLint;
                         Y      : in GLint;
                         Width  : in GLsizei;
                         Height : in GLsizei );

   procedure glShadeModel ( Mode : in GLenum );

   procedure glStencilFunc ( Func : in GLenum;
                             Ref  : in GLint;
                             Mask : in GLuint );

   procedure glStencilMask (mask : in GLuint);

   procedure glStencilOp ( Fail  : in GLenum;
                           Zfail : in GLenum;
                           Zpass : in GLenum );

   procedure glTexCoordPointer ( Size    : in GLint;
                                 C_Type  : in GLenum;
                                 Stride  : in GLsizei;
                                 Pointer : in System.Address );

   procedure glTexEnvfv ( Target : in GLenum;
                          Pname  : in GLenum;
                          Params : access GLfloat );

   procedure glTexEnvi ( Target : in GLenum;
                         Pname  : in GLenum;
                         Param  : in GLint );

   procedure glTexImage2D ( Target         : in GLenum;
                            Level          : in GLint;
                            Internalformat : in GLint;
                            Width          : in GLsizei;
                            Height         : in GLsizei;
                            Border         : in GLint;
                            Format         : in GLenum;
                            C_Type         : in GLenum;
                            Pixels         : in System.Address );

   procedure glTexParameteri ( Target : in GLenum;
                               Pname  : in GLenum;
                               Param  : in GLint );

   procedure glTexSubImage2D ( Target  : in GLenum;
                               Level   : in GLint;
                               Xoffset : in GLint;
                               Yoffset : in GLint;
                               Width   : in GLsizei;
                               Height  : in GLsizei;
                               Format  : in GLenum;
                               C_Type  : in GLenum;
                               Pixels  : in System.Address);

   procedure glTranslatef ( X : in GLfloat;
                            Y : in GLfloat;
                            Z : in GLfloat );

   procedure glVertex2f ( X : in GLfloat;
                          Y : in GLfloat );

   procedure glVertex2fv ( V : access GLfloat );

   procedure glVertex3f ( X : in GLfloat;
                          Y : in GLfloat;
                          Z : in GLfloat );

   procedure glVertex3fv ( V : access GLfloat );

   procedure glVertexPointer ( Size    : in GLint;
                               C_Type  : in GLenum;
                               Stride  : in GLsizei;
                               Pointer : in System.Address);

   procedure glViewport ( X      : in GLint;
                          Y      : in GLint;
                          Width  : in GLsizei;
                          Height : in GLsizei );

-------------------------------------------------------------------------------

private

   pragma Import (StdCall, glBitmap, "glBitmap");
   pragma Import (StdCall, glActiveTexture, "glActiveTexture");
   pragma Import (StdCall, glAlphaFunc, "glAlphaFunc");
   pragma Import (StdCall, glBegin, "glBegin");
   pragma Import (StdCall, glBindTexture, "glBindTexture");
   pragma Import (StdCall, glBlendFunc, "glBlendFunc");
   pragma Import (StdCall, glCallLists, "glCallLists");
   pragma Import (StdCall, glClear, "glClear");
   pragma Import (StdCall, glClearColor, "glClearColor");
   pragma Import (StdCall, glClearDepthf, "glClearDepthf");
   pragma Import (StdCall, glClearStencil, "glClearStencil");
   pragma Import (StdCall, glClientActiveTexture, "glClientActiveTexture");
   pragma Import (StdCall, glColor4f, "glColor4f");
   pragma Import (StdCall, glColor4fv, "glColor4fv");
   pragma Import (StdCall, glColor4ub, "glColor4ub");
   pragma Import (StdCall, glColorMask, "glColorMask");
   pragma Import (StdCall, glColorPointer, "glColorPointer");
   pragma Import (StdCall, glColorSubTableEXT, "glColorSubTableEXT");
   pragma Import (StdCall, glColorTableEXT, "glColorTableEXT");
   pragma Import (StdCall, glCopyPixels, "glCopyPixels");
   pragma Import (StdCall, glCullFace, "glCullFace");
   pragma Import (StdCall, glDepthFunc, "glDepthFunc");
   pragma Import (StdCall, glDepthMask, "glDepthMask");
   pragma Import (StdCall, glDepthRangef, "glDepthRangef");
   pragma Import (StdCall, glDisable, "glDisable");
   pragma Import (StdCall, glDisableClientState, "glDisableClientState");
   pragma Import (StdCall, glDrawArrays, "glDrawArrays");
   pragma Import (StdCall, glDrawElements, "glDrawElements");
   pragma Import (StdCall, glDrawPixels, "glDrawPixels");
   pragma Import (StdCall, glEnable, "glEnable");
   pragma Import (StdCall, glEnableClientState, "glEnableClientState");
   pragma Import (StdCall, glEnd, "glEnd");
   pragma Import (StdCall, glEndList, "glEndList");
   pragma Import (StdCall, glFinish, "glFinish");
   pragma Import (StdCall, glFlush, "glFlush");
   pragma Import (StdCall, glFrontFace, "glFrontFace");
   pragma Import (StdCall, glFrustumf, "glFrustumf");
   pragma Import (StdCall, glGenLists, "glGenLists");
   pragma Import (StdCall, glGenTextures, "glGenTextures");
   pragma Import (StdCall, glGetError, "glGetError");
   pragma Import (StdCall, glGetBooleanv, "glGetBooleanv");
   pragma Import (StdCall, glGetColorTableEXT, "glGetColorTableEXT");
   pragma Import (StdCall, glGetColorTableParameterivEXT, "glGetColorTableParameterivEXT");
   pragma Import (StdCall, glGetFloatv, "glGetFloatv");
   pragma Import (StdCall, glGetIntegerv, "glGetIntegerv");
   pragma Import (StdCall, glGetLightfv, "glGetLightfv");
   pragma Import (StdCall, glGetMaterialfv, "glGetMaterialfv");
   pragma Import (StdCall, glGetPointerv, "glGetPointerv");
   pragma Import (StdCall, glGetPolygonStipple, "glGetPolygonStipple");
   pragma Import (StdCall, glGetTexEnvfv, "glGetTexEnvfv");
   pragma Import (StdCall, glGetTexEnviv, "glGetTexEnviv");
   pragma Import (StdCall, glGetTexParameteriv, "glGetTexParameteriv");
   pragma Import (StdCall, glGetString, "glGetString");
   pragma Import (StdCall, glHint, "glHint");
   pragma Import (StdCall, glIsEnabled, "glIsEnabled");
   pragma Import (StdCall, glLightfv, "glLightfv");
   pragma Import (StdCall, glLightModelfv, "glLightModelfv");
   pragma Import (StdCall, glLineStipple, "glLineStipple");
   pragma Import (StdCall, glLineWidth, "glLineWidth");
   pragma Import (StdCall, glListBase, "glListBase");
   pragma Import (StdCall, glLoadIdentity, "glLoadIdentity");
   pragma Import (StdCall, glLoadMatrixf, "glLoadMatrixf");
   pragma Import (StdCall, glMaterialf, "glMaterialf");
   pragma Import (StdCall, glMaterialfv, "glMaterialfv");
   pragma Import (StdCall, glMatrixMode, "glMatrixMode");
   pragma Import (StdCall, glMultMatrixf, "glMultMatrixf");
   pragma Import (StdCall, glMultiTexCoord2f, "glMultiTexCoord2f");
   pragma Import (StdCall, glMultiTexCoord2fv, "glMultiTexCoord2fv");
   pragma Import (StdCall, glNewList, "glNewList");
   pragma Import (StdCall, glNormal3f, "glNormal3f");
   pragma Import (StdCall, glNormal3fv, "glNormal3fv");
   pragma Import (StdCall, glNormalPointer, "glNormalPointer");
   pragma Import (StdCall, glOrthof, "glOrthof");
   pragma Import (StdCall, glPixelStorei, "glPixelStorei");
   pragma Import (StdCall, glPointSize, "glPointSize");
   pragma Import (StdCall, glPolygonOffset, "glPolygonOffset");
   pragma Import (StdCall, glPolygonStipple, "glPolygonStipple");
   pragma Import (StdCall, glPopMatrix, "glPopMatrix");
   pragma Import (StdCall, glPushMatrix, "glPushMatrix");
   pragma Import (StdCall, glRasterPos3f, "glRasterPos3f");
   pragma Import (StdCall, glReadPixels, "glReadPixels");
   pragma Import (StdCall, glRotatef, "glRotatef");
   pragma Import (StdCall, glScalef, "glScalef");
   pragma Import (StdCall, glScissor, "glScissor");
   pragma Import (StdCall, glShadeModel, "glShadeModel");
   pragma Import (StdCall, glStencilFunc, "glStencilFunc");
   pragma Import (StdCall, glStencilMask, "glStencilMask");
   pragma Import (StdCall, glStencilOp, "glStencilOp");
   pragma Import (StdCall, glTexCoordPointer, "glTexCoordPointer");
   pragma Import (StdCall, glTexEnvfv, "glTexEnvfv");
   pragma Import (StdCall, glTexEnvi, "glTexEnvi");
   pragma Import (StdCall, glTexImage2D, "glTexImage2D");
   pragma Import (StdCall, glTexParameteri, "glTexParameteri");
   pragma Import (StdCall, glTexSubImage2D, "glTexSubImage2D");
   pragma Import (StdCall, glTranslatef, "glTranslatef");
   pragma Import (StdCall, glVertex2f, "glVertex2f");
   pragma Import (StdCall, glVertex2fv, "glVertex2fv");
   pragma Import (StdCall, glVertex3f, "glVertex3f");
   pragma Import (StdCall, glVertex3fv, "glVertex3fv");
   pragma Import (StdCall, glVertexPointer, "glVertexPointer");
   pragma Import (StdCall, glViewport, "glViewport");

end GLSC;
