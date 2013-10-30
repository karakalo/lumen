package body openGL.Math
is
   use type Real;


   --  Vectors
   --

   function "*" (Left : in Vector_3;   Right : in Vector_3) return Vector_3
   is
   begin
      return (1 => Left (2) * Right (3)  -  Left (3) * Right (2),
              2 => Left (3) * Right (1)  -  Left (1) * Right (3),
              3 => Left (1) * Right (2)  -  Left (2) * Right (1));
   end "*";




   function "*" (Left : in Vector_3;   Right : in Vector_3) return Real
   is
   begin
      return   Left (1) * Right (1)
             + Left (2) * Right (2)
             + Left (3) * Right (3);
   end "*";



   function "/" (Left : in Vector_3;   Right : in Vector_3) return Vector_3
   is
   begin
      return (Left (1) / Right (1),
              Left (2) / Right (2),
              Left (3) / Right (3));
   end "/";



   function "/" (Left : in Vector_3;     Right : in Real) return Vector_3
   is
   begin
      return (Left (1) / Right,
              Left (2) / Right,
              Left (3) / Right);
   end "/";



   function Norm_2 (Self : in Vector_3) return Real
   is
      the_Norm_2 : Real;
   begin
      the_Norm_2 :=              Self (1) * Self (1);
      the_Norm_2 := the_Norm_2 + Self (2) * Self (2);
      the_Norm_2 := the_Norm_2 + Self (3) * Self (3);

      return the_Norm_2;
   end Norm_2;



   function Norm (Self : in Vector_3) return Real
   is
   begin
      return Sqrt (Norm_2 (Self));
   end Norm;



   function Midpoint (Left : in Vector_3;   Right : in Vector_3) return Vector_3
   is
   begin
      return ((Left (1) + Right (1)) * 0.5,
              (Left (2) + Right (2)) * 0.5,
              (Left (3) + Right (3)) * 0.5);
   end Midpoint;




   function "+" (Left : in Vector_3;   Right : in Vector_3) return Vector_3
   is
   begin
      return (Left (1) + Right (1),
              Left (2) + Right (2),
              Left (3) + Right (3));
   end "+";



   function "-" (Left : in Vector_3;   Right : in Vector_3) return Vector_3
   is
   begin
      return (Left (1) - Right (1),
              Left (2) - Right (2),
              Left (3) - Right (3));
   end "-";



   function "-" (Self : in Vector_3) return Vector_3
   is
   begin
      return (-Self (1),  -Self (2),  -Self (3));
   end "-";



   function "*" (Left : in Vector_3;   Right : in Real) return Vector_3
   is
   begin
      return (Left (1) * Right,
              Left (2) * Right,
              Left (3) * Right);
   end "*";



   function "*" (Left : in Real;   Right : in Vector_3) return Vector_3
   is
   begin
      return Right * Left;
   end "*";



   function Scaled (Self : in Vector_3;   By : in Real) return Vector_3
   is
   begin
      return (Self (1) * By,
              Self (2) * By,
              Self (3) * By);
   end Scaled;



   function Scaled (Self : in Vector_3;   By : in Vector_3) return Vector_3
   is
   begin
      return (Self (1) * By (1),
              Self (2) * By (2),
              Self (3) * By (3));
   end Scaled;



   function Scaled (Self : in Vector_3_array;   By : in Vector_3) return Vector_3_array
   is
      Result : Vector_3_array (Self'Range);
   begin
      for Each in Result'Range loop
         Result (Each) := Scaled (Self (Each),  By);
      end loop;

      return Result;
   end Scaled;



   function Interpolated (v0, v1 : in Vector_3;
                          rt     : in Real) return Vector_3
   is
      s : constant Real := 1.0 - rt;
   begin
      return (s * v0 (1) + rt * v1 (1),
              s * v0 (2) + rt * v1 (2),
              s * v0 (3) + rt * v1 (3));
   end Interpolated;



   procedure normalise (Self : in out Vector_3)
   is
      inv_Norm : Real := 1.0 / Norm (Self);
   begin
      for Each in self'Range loop
         Self (Each) := Self (Each) * inv_Norm;
      end loop;
   end normalise;



   function Normalised (Self : in Vector_3) return Vector_3
   is
      Result : Vector_3 := Self;
   begin
      normalise (Result);
      return Result;
   end Normalised;



   function Min (Left, Right : in Vector_3) return Vector_3
   is
   begin
      return (Real'Min (Left (1),  Right (1)),
              Real'Min (Left (2),  Right (2)),
              Real'Min (Left (3),  Right (3)));
   end Min;



   function Max (Left, Right : in Vector_3) return Vector_3
   is
   begin
      return (Real'Max (Left (1),  Right (1)),
              Real'Max (Left (2),  Right (2)),
              Real'Max (Left (3),  Right (3)));
   end Max;



   --  Bounds
   --

   function bounding_Box (Self : Sites) return Bounds
   is
      the_Bounds : Bounds := null_Bounds;
   begin
      for Each in Self'Range loop
         the_Bounds.Lower (1) := Real'Min  (the_Bounds.Lower (1),  Self (Each)(1));
         the_Bounds.Lower (2) := Real'Min  (the_Bounds.Lower (2),  Self (Each)(2));
         the_Bounds.Lower (3) := Real'Min  (the_Bounds.Lower (3),  Self (Each)(3));

         the_Bounds.Upper (1) := Real'Max  (the_Bounds.Upper (1),  Self (Each)(1));
         the_Bounds.Upper (2) := Real'Max  (the_Bounds.Upper (2),  Self (Each)(2));
         the_Bounds.Upper (3) := Real'Max  (the_Bounds.Upper (3),  Self (Each)(3));
      end loop;


      return the_Bounds;
   end bounding_Box;



   function Extent (Self : in Bounds;   Dimension : in Index_t) return Real
   is
   begin
      return Self.Upper (Dimension) - Self.Lower (Dimension);
   end Extent;



   function "+" (Left : in Bounds;   Right : in Vector_3) return Bounds
   is
   begin
      return (Left.Lower + Right,
              Left.Upper + Right);
   end "+";



   function "or" (Left : in Bounds;   Right : in Vector_3) return Bounds
   is
      Result : Bounds;
   begin
      for i in Right'Range
      loop
         if Right (i) < Left.Lower (i) then
            Result.Lower (i) := Right (i);
         else
            Result.Lower (i) := Left.Lower (i);
         end if;

         if Right (i) > Left.Upper (i) then
            Result.Upper (i) := Right (i);
         else
            Result.Upper (i) := Left.Upper (i);
         end if;
      end loop;

      return Result;
   end "or";



   function "or" (Left : in Bounds;   Right : in Bounds) return Bounds
   is
      Result : Bounds := Left or Right.Lower;
   begin
      Result := Result or Right.Upper;
      return Result;
   end "or";



   use type Index_t, long_Index_t;



   --  Rotations
   --

   function x_Rotation_from (the_Angle : in Real) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;
   begin
      the_matrix (1, 1) := 1.0;
      the_matrix (1, 2) := 0.0;
      the_matrix (1, 3) := 0.0;

      the_matrix (2, 1) := 0.0;
      the_matrix (2, 2) := cos (the_Angle);
      the_matrix (2, 3) := -sin (the_Angle);

      the_matrix (3, 1) := 0.0;
      the_matrix (3, 2) := sin (the_Angle);
      the_matrix (3, 3) := cos (the_Angle);

      return the_Matrix;
   end x_Rotation_from;



   function y_Rotation_from (the_Angle : in Real) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;
   begin
      the_Matrix (1, 1) := cos (the_Angle);
      the_Matrix (1, 2) := 0.0;
      the_Matrix (1, 3) := -sin (the_Angle);

      the_Matrix (2, 1) := 0.0;
      the_Matrix (2, 2) := 1.0;
      the_Matrix (2, 3) := 0.0;

      the_Matrix (3, 1) := sin (the_Angle);
      the_Matrix (3, 2) := 0.0;
      the_Matrix (3, 3) := cos (the_Angle);

      return the_Matrix;
   end y_Rotation_from;



   function z_Rotation_from (the_Angle : in Real) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;
   begin
      the_Matrix (1, 1) := cos (the_Angle);
      the_Matrix (1, 2) := -sin (the_Angle);
      the_Matrix (1, 3) := 0.0;

      the_Matrix (2, 1) := sin (the_Angle);
      the_Matrix (2, 2) := cos (the_Angle);
      the_Matrix (2, 3) := 0.0;

      the_Matrix (3, 1) := 0.0;
      the_Matrix (3, 2) := 0.0;
      the_Matrix (3, 3) := 1.0;

      return the_Matrix;
   end z_Rotation_from;



   --  Matrices
   --

   function "*" (Left : Matrix_3x3;   Right : Vector_3) return Vector_3
   is
   begin
      return (left (1, 1) * right (1)  +  left (1, 2) * right (2)  +  left (1, 3) * right (3),
              left (2, 1) * right (1)  +  left (2, 2) * right (2)  +  left (2, 3) * right (3),
              left (3, 1) * right (1)  +  left (3, 2) * right (2)  +  left (3, 3) * right (3));
   end "*";



   function "*" (Left : Vector_3;   Right : Matrix_3x3) return Vector_3
   is
   begin
      return Right * Left;
   end "*";



   function "*" (Left : Matrix_3x3;   Right : Matrix_3x3) return Matrix_3x3
   is
      r  : Real;
      AB : Matrix_3x3;
   begin
      for i in Left'First (1) .. Left'Last (1)
      loop
         for j in Left'First (2) .. Left'Last (2)
         loop
            r := 0.0;

            for k in Left'First (1) .. Left'Last (1)
            loop
               r := r  +  (Left (i, k)  *  Right (k, j));
            end loop;

            AB (i, j) := r;
         end loop;
      end loop;

      return AB;
   end "*";


end openGL.Math;
