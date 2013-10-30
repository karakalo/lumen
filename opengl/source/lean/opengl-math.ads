
package openGL.Math
--
--  Provides opengl specific math.
--
is
   pragma Pure;


   Pi : constant := 3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;


   --  Vector_3
   --

   Origin_3d : constant Vector_3;


   function  "+" (Left : in Vector_3;     Right : in Vector_3) return Vector_3;
   function  "-" (Left : in Vector_3;     Right : in Vector_3) return Vector_3;

   function  "-" (Self : in Vector_3) return Vector_3;

   function  "*" (Left : in Vector_3;     Right : in Vector_3) return Vector_3;
   function  "*" (Left : in Vector_3;     Right : in Vector_3) return Real;
   function  "*" (Left : in Vector_3;     Right : in Real)     return Vector_3;
   function  "*" (Left : in Real;         Right : in Vector_3) return Vector_3;

   function  "/" (Left : in Vector_3;     Right : in Vector_3) return Vector_3;
   function  "/" (Left : in Vector_3;     Right : in Real)     return Vector_3;

   function  Min (Left : in Vector_3;     Right : in Vector_3) return Vector_3;
   function  Max (Left : in Vector_3;     Right : in Vector_3) return Vector_3;

   function  Norm_2     (Self : in     Vector_3) return Real;
   function  Norm       (Self : in     Vector_3) return Real;
   function  Normalised (Self : in     Vector_3) return Vector_3;
   procedure normalise  (Self : in out Vector_3);

   function  Midpoint (Left, Right : Vector_3) return Vector_3;

   function  Scaled (Self : in Vector_3;         By : in Real)     return Vector_3;
   function  Scaled (Self : in Vector_3;         By : in Vector_3) return Vector_3;
   function  Scaled (Self : in Vector_3_array;   By : in Vector_3) return Vector_3_array;

   function  Interpolated (v0, v1 : in Vector_3;
                           rt     : in Real) return Vector_3;


   --  Matrix_3x3
   --

   function "*" (Left : in Matrix_3x3;   Right : in Vector_3)   return Vector_3;
   function "*" (Left : in Vector_3;     Right : in Matrix_3x3) return Vector_3;
   function "*" (Left : in Matrix_3x3;   Right : in Matrix_3x3) return Matrix_3x3;

   Identity_3x3 : constant matrix_3x3;



   --  Matrix_4x4
   --

   Identity_4x4 : constant matrix_4x4;



   --  Bounds
   --

   function bounding_Box (Self : in Sites) return Bounds;

   function "+"  (Left : in Bounds;   Right : in Vector_3) return Bounds;   -- Applies an offset to the bounds.

   function "or" (Left : in Bounds;   Right : in Vector_3) return Bounds;
   --
   --  Returns the bounds expanded to include the vector.

   function "or" (Left : in Bounds;   Right : in Bounds) return Bounds;
   --
   --  Returns the bounds expanded to include both Left and Right.

   function Extent (Self : in Bounds;   Dimension : in Index_t) return Real;



   --  Rotations
   --

   function x_Rotation_from (the_Angle : in Real) return Matrix_3x3;
   function y_Rotation_from (the_Angle : in Real) return Matrix_3x3;
   function z_Rotation_from (the_Angle : in Real) return Matrix_3x3;



private

   Origin_3d    : constant Vector_3   := (0.0, 0.0, 0.0);

   Identity_3x3 : constant matrix_3x3 := ((1.0, 0.0, 0.0),
                                          (0.0, 1.0, 0.0),
                                          (0.0, 0.0, 1.0));

   Identity_4x4 : constant matrix_4x4 := ((1.0, 0.0, 0.0, 0.0),
                                          (0.0, 1.0, 0.0, 0.0),
                                          (0.0, 0.0, 1.0, 0.0),
                                          (0.0, 0.0, 0.0, 1.0));
end openGL.Math;
