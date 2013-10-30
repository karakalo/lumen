with
     GL,
     ada.numerics.generic_elementary_Functions,
     ada.Streams;

package openGL
--
--  Provides a namespace and set of core types.
--
is
   pragma Pure;

   Error : exception;


   -----------
   --  Indices
   -----------
   type      Index_t is range 0 .. 2**16 - 1;
   type long_Index_t is range 0 .. 2**32 - 1;

   type      Indices is array (long_Index_t range <>) of      Index_t;
   type long_Indices is array (long_Index_t range <>) of long_Index_t;



   --------
   --  Math
   --------

   --  Real
   --
   subtype Real is gl.GLfloat;

   package real_Functions is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use     real_Functions;

   function almost_Zero (X : Real) return Boolean;


   --  Extents
   --
   type Extent_2d is
      record
         Width  : Natural;
         Height : Natural;
      end record;


   --  Vectors
   --
   type Vector         is array (Index_t range <>) of aliased Real;

   type Vector_2       is new Vector (1 .. 2);
   type Vector_3       is new Vector (1 .. 3);
   type Vector_4       is new Vector (1 .. 4);

   type Vector_3_array is array (Index_t range <>) of aliased Vector_3;


   --  Matrices
   --
   type Matrix     is array (Index_t range <>,  Index_t range <>) of aliased Real;

   type Matrix_2x2 is new Matrix (1 .. 2,  1 .. 2);
   type Matrix_3x3 is new Matrix (1 .. 3,  1 .. 3);
   type Matrix_4x4 is new Matrix (1 .. 4,  1 .. 4);

   Identity_4x4 : constant Matrix_4x4;


   --  Height Maps  -- tbd: belongs here ?
   --
   type height_Map is array (Index_t range <>,
                             Index_t range <>) of aliased Real;


   function  scaled (Self : in     height_Map;   By : in Real) return height_Map;
   procedure scale  (Self : in out height_Map;   By : in Real);

   function  height_Extent (Self : in height_Map) return Vector_2;
   --
   --  Returns the min and max height.


   type index_Pair is array (1 .. 2) of Index_t;

   function  Region (Self : in height_Map;   Rows, Cols : in index_Pair) return height_Map;
   --
   --  Returns the submatrix indicated via Rows & Cols.


   ------------
   --  Geometry
   ------------

   subtype Site      is Vector_3;                   -- A position in 3d space.
   subtype Sites     is Vector_3_array;

   subtype Normal    is Vector_3;
   subtype Normals   is Vector_3_array;

   type Bounds is
      record
         Lower : Site;
         Upper : Site;
      end record;

   null_Bounds : constant Bounds;


   ---------
   --  Color
   ---------

   subtype  grey_Value is gl.GLubyte;
   subtype color_Value is gl.GLubyte;

   Opaque : constant color_Value;
   Lucid  : constant color_Value;

   function to_color_Value (Self : in Real)        return color_Value;
   function to_Real        (Self : in color_Value) return Real;


   type Color is
      record
         Red   : aliased color_Value;
         Green :         color_Value;
         Blue  :         color_Value;
      end record;

   type Colors is array (Index_t range <>) of Color;


   type lucid_Color is
      record
         Primary : Color;
         Opacity : color_Value;
      end record;

   type lucid_Colors is array (Index_t range <>) of lucid_Color;



   ----------
   --  Images
   ----------

   type  grey_Image is array (Index_t range <>, Index_t range <>) of aliased grey_Value;
   type       Image is array (Index_t range <>, Index_t range <>) of aliased Color;
   type lucid_Image is array (Index_t range <>, Index_t range <>) of aliased lucid_Color;

   function to_Image (From : in lucid_Image) return Image;



   -----------
   --  Texture
   -----------

   --  Coordinates
   --
   type Coordinate_1D is
      record
         S : aliased Real;
      end record;

   type Coordinates_1D is array (Index_t range <>) of Coordinate_1D;


   type Coordinate_2D is
      record
         S, T : aliased Real;
      end record;

   type Coordinates_2D is array (Index_t range <>) of aliased Coordinate_2D;


   --  Transforms
   --

   type texture_Transform is
     record
       Offset : Real;
       Scale  : Real;
     end record;


   type texture_Transform_1d is
     record
       S : texture_Transform;
     end record;

   type texture_Transform_2d is
     record
       S : texture_Transform;
       T : texture_Transform;
     end record;



private
   pragma Pack (Indices);     -- NB: Important !


   null_Bounds  : constant Bounds     := (lower => (Real'Last,  Real'Last,  Real'Last),
                                          upper => (Real'First, Real'First, Real'First));

   Identity_4x4 : constant Matrix_4x4 := ((1.0, 0.0, 0.0, 0.0),
                                          (0.0, 1.0, 0.0, 0.0),
                                          (0.0, 0.0, 1.0, 0.0),
                                          (0.0, 0.0, 0.0, 1.0));

   --  Color
   --
   Opaque : constant color_Value := color_Value'Last;
   Lucid  : constant color_Value := color_Value'First;

   function to_Color (R, G, B : in Real) return Color;



   --  Streams
   --
   procedure height_Map_write (Stream : not null access Ada.Streams.Root_Stream_Type'Class;   Item : in  height_Map);
   --     for height_Map'write use height_Map_write;

   procedure height_Map_read (Stream : not null access Ada.Streams.Root_Stream_Type'Class;    Item : out height_Map);
   --     for height_Map'read use height_Map_read;


   procedure height_Map_output (Stream : not null access Ada.Streams.Root_Stream_Type'Class;   Self : in  height_Map);
   --     for height_Map'output use height_Map_output;

   function height_Map_input (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return height_Map;
   --     for height_Map'input use height_Map_input;

end openGL;
