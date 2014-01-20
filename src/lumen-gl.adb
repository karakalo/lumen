
-- Lumen.GL -- Lumen's own thin OpenGL bindings
--
-- Chip Richards, NiEstu, Phoenix AZ, Summer 2010

-- This code is covered by the ISC License:
--
-- Copyright © 2010, NiEstu
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- The software is provided "as is" and the author disclaims all warranties
-- with regard to this software including all implied warranties of
-- merchantability and fitness. In no event shall the author be liable for any
-- special, direct, indirect, or consequential damages or any damages
-- whatsoever resulting from loss of use, data or profits, whether in an
-- action of contract, negligence or other tortious action, arising out of or
-- in connection with the use or performance of this software.

with Interfaces.C.Strings;

package body Lumen.GL is

   ---------------------------------------------------------------------------

   -- Misc stuff
   function Get_String (Name : Enum) return String is

      use type Interfaces.C.Strings.chars_ptr;

      function glGetString (Name : Enum) return Interfaces.C.Strings.chars_ptr;
      pragma Import (StdCall, glGetString, "glGetString");

      Ptr : constant Interfaces.C.Strings.chars_ptr := glGetString (Name);

   begin  -- Get_String
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Ptr);
      end if;
   end Get_String;

   function Get_String (Name  : Enum;
                        Index : Int) return String is

      use type Interfaces.C.Strings.chars_ptr;

      function glGetStringi (Name : Enum;  Index : Int) return Interfaces.C.Strings.chars_ptr;
      pragma Import (StdCall, glGetStringi, "glGetStringi");

      Ptr : constant Interfaces.C.Strings.chars_ptr := glGetStringi (Name, Index);

   begin  -- Get_String
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Ptr);
      end if;
   end Get_String;

   ---------------------------------------------------------------------------

   -- Transformations
   procedure Rotate (Angle : in Double;
                     X     : in Double;
                     Y     : in Double;
                     Z     : in Double) is
      procedure glRotated (Angle : in Double;
                           X     : in Double;
                           Y     : in Double;
                           Z     : in Double);
      pragma Import (StdCall, glRotated, "glRotated");
   begin  -- Rotate
      glRotated (Angle, X, Y, Z);
   end Rotate;

   procedure Rotate (Angle : in Float;
                     X     : in Float;
                     Y     : in Float;
                     Z     : in Float) is
      procedure glRotatef (Angle : in Float;
                           X     : in Float;
                           Y     : in Float;
                           Z     : in Float);
      pragma Import (StdCall, glRotatef, "glRotatef");
   begin  -- Rotate
      glRotatef (Angle, X, Y, Z);
   end Rotate;

   procedure Scale (X : in Double;
                    Y : in Double;
                    Z : in Double) is
      procedure glScaled (X : in Double;
                          Y : in Double;
                          Z : in Double);
      pragma Import (StdCall, glScaled, "glScaled");
   begin  -- Scale
      glScaled (X, Y, Z);
   end Scale;

   procedure Scale (X : in Float;
                    Y : in Float;
                    Z : in Float) is
      procedure glScalef (X : in Float;
                          Y : in Float;
                          Z : in Float);
      pragma Import (StdCall, glScalef, "glScalef");
   begin  -- Scale
      glScalef (X, Y, Z);
   end Scale;

   procedure Translate (X : in Double;
                        Y : in Double;
                        Z : in Double) is
      procedure glTranslated (X : in Double;
                              Y : in Double;
                              Z : in Double);
      pragma Import (StdCall, glTranslated, "glTranslated");
   begin  -- Translate
      glTranslated (X, Y, Z);
   end Translate;

   procedure Translate (X : in Float;
                        Y : in Float;
                        Z : in Float) is
      procedure glTranslatef (X : in Float;
                              Y : in Float;
                              Z : in Float);
      pragma Import (StdCall, glTranslatef, "glTranslatef");
   begin  -- Translate
      glTranslatef (X, Y, Z);
   end Translate;

   ---------------------------------------------------------------------------

   -- Matrix operations
   procedure Load_Matrix (M : in Float_Matrix) is
      procedure glLoadMatrixf (M : in System.Address);
      pragma Import (StdCall, glLoadMatrixf, "glLoadMatrixf");
   begin  -- LoadMatrix
      glLoadMatrixf (M'Address);
   end Load_Matrix;

   procedure Load_Matrix (M : in Double_Matrix) is
      procedure glLoadMatrixd (M : in System.Address);
      pragma Import (StdCall, glLoadMatrixd, "glLoadMatrixd");
   begin  -- LoadMatrix
      glLoadMatrixd (M'Address);
   end Load_Matrix;

   procedure Mult_Matrix (M : in Float_Matrix) is
      procedure glMultMatrixf (M : in System.Address);
      pragma Import (StdCall, glMultMatrixf, "glMultMatrixf");
   begin  -- MultMatrix
      glMultMatrixf (M'Address);
   end Mult_Matrix;

   procedure Mult_Matrix (M : in Double_Matrix) is
      procedure glMultMatrixd (M : in System.Address);
      pragma Import (StdCall, glMultMatrixd, "glMultMatrixd");
   begin  -- MultMatrix
      glMultMatrixd (M'Address);
   end Mult_Matrix;

   ---------------------------------------------------------------------------

   -- Component color
   procedure Color (Red   : in Byte;
                    Green : in Byte;
                    Blue  : in Byte) is
      procedure glColor3b (Red   : in Byte;
                           Green : in Byte;
                           Blue  : in Byte);
      pragma Import (StdCall, glColor3b, "glColor3b");
   begin  -- Color
      glColor3b (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Short;
                    Green : in Short;
                    Blue  : in Short) is
      procedure glColor3s (Red   : in Short;
                           Green : in Short;
                           Blue  : in Short);
      pragma Import (StdCall, glColor3s, "glColor3s");
   begin  -- Color
      glColor3s (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Int;
                    Green : in Int;
                    Blue  : in Int) is
      procedure glColor3i (Red   : in Int;
                           Green : in Int;
                           Blue  : in Int);
      pragma Import (StdCall, glColor3i, "glColor3i");
   begin  -- Color
      glColor3i (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Float;
                    Green : in Float;
                    Blue  : in Float) is
      procedure glColor3f (Red   : in Float;
                           Green : in Float;
                           Blue  : in Float);
      pragma Import (StdCall, glColor3f, "glColor3f");
   begin  -- Color
      glColor3f (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Double;
                    Green : in Double;
                    Blue  : in Double) is
      procedure glColor3d (Red   : in Double;
                           Green : in Double;
                           Blue  : in Double);
      pragma Import (StdCall, glColor3d, "glColor3d");
   begin  -- Color
      glColor3d (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in UByte;
                    Green : in UByte;
                    Blue  : in UByte) is
      procedure glColor3ub (Red   : in UByte;
                            Green : in UByte;
                            Blue  : in UByte);
      pragma Import (StdCall, glColor3ub, "glColor3ub");
   begin  -- Color
      glColor3ub (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in UShort;
                    Green : in UShort;
                    Blue  : in UShort) is
      procedure glColor3us (Red   : in UShort;
                            Green : in UShort;
                            Blue  : in UShort);
      pragma Import (StdCall, glColor3us, "glColor3us");
   begin  -- Color
      glColor3us (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in UInt;
                    Green : in UInt;
                    Blue  : in UInt) is
      procedure glColor3ui (Red   : in UInt;
                            Green : in UInt;
                            Blue  : in UInt);
      pragma Import (StdCall, glColor3ui, "glColor3ui");
   begin  -- Color
      glColor3ui (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Byte;
                    Green : in Byte;
                    Blue  : in Byte;
                    Alpha : in Byte) is
      procedure glColor4b (Red   : in Byte;
                           Green : in Byte;
                           Blue  : in Byte;
                           Alpha : in Byte);
      pragma Import (StdCall, glColor4b, "glColor4b");
   begin  -- Color
      glColor4b (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (Red   : in Short;
                    Green : in Short;
                    Blue  : in Short;
                    Alpha : in Short) is
      procedure glColor4s (Red   : in Short;
                           Green : in Short;
                           Blue  : in Short;
                           Alpha : in Short);
      pragma Import (StdCall, glColor4s, "glColor4s");
   begin  -- Color
      glColor4s (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (Red   : in Int;
                    Green : in Int;
                    Blue  : in Int;
                    Alpha : in Int) is
      procedure glColor4i (Red   : in Int;
                           Green : in Int;
                           Blue  : in Int;
                           Alpha : in Int);
      pragma Import (StdCall, glColor4i, "glColor4i");
   begin  -- Color
      glColor4i (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (Red   : in Float;
                    Green : in Float;
                    Blue  : in Float;
                    Alpha : in Float) is
      procedure glColor4f (Red   : in Float;
                           Green : in Float;
                           Blue  : in Float;
                           Alpha : in Float);
      pragma Import (StdCall, glColor4f, "glColor4f");
   begin  -- Color
      glColor4f (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (Red   : in Double;
                    Green : in Double;
                    Blue  : in Double;
                    Alpha : in Double) is
      procedure glColor4d (Red   : in Double;
                           Green : in Double;
                           Blue  : in Double;
                           Alpha : in Double);
      pragma Import (StdCall, glColor4d, "glColor4d");
   begin  -- Color
      glColor4d (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (Red   : in UByte;
                    Green : in UByte;
                    Blue  : in UByte;
                    Alpha : in UByte) is
      procedure glColor4ub (Red   : in UByte;
                            Green : in UByte;
                            Blue  : in UByte;
                            Alpha : in UByte);
      pragma Import (StdCall, glColor4ub, "glColor4ub");
   begin  -- Color
      glColor4ub (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (Red   : in UShort;
                    Green : in UShort;
                    Blue  : in UShort;
                    Alpha : in UShort) is
      procedure glColor4us (Red   : in UShort;
                            Green : in UShort;
                            Blue  : in UShort;
                            Alpha : in UShort);
      pragma Import (StdCall, glColor4us, "glColor4us");
   begin  -- Color
      glColor4us (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (Red   : in UInt;
                    Green : in UInt;
                    Blue  : in UInt;
                    Alpha : in UInt) is
      procedure glColor4ui (Red   : in UInt;
                            Green : in UInt;
                            Blue  : in UInt;
                            Alpha : in UInt);
      pragma Import (StdCall, glColor4ui, "glColor4ui");
   begin  -- Color
      glColor4ui (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (V : in Bytes_3) is
      procedure glColor3bv (V : in Bytes_3);
      pragma Import (StdCall, glColor3bv, "glColor3bv");
   begin  -- Color
      glColor3bv (V);
   end Color;

   procedure Color (V : in Bytes_4) is
      procedure glColor4bv (V : in Bytes_4);
      pragma Import (StdCall, glColor4bv, "glColor4bv");
   begin  -- Color
      glColor4bv (V);
   end Color;

   procedure Color (V : in Shorts_3) is
      procedure glColor3sv (V : in Shorts_3);
      pragma Import (StdCall, glColor3sv, "glColor3sv");
   begin  -- Color
      glColor3sv (V);
   end Color;

   procedure Color (V : in Shorts_4) is
      procedure glColor4sv (V : in Shorts_4);
      pragma Import (StdCall, glColor4sv, "glColor4sv");
   begin  -- Color
      glColor4sv (V);
   end Color;

   procedure Color (V : in Ints_3) is
      procedure glColor3iv (V : in Ints_3);
      pragma Import (StdCall, glColor3iv, "glColor3iv");
   begin  -- Color
      glColor3iv (V);
   end Color;

   procedure Color (V : in Ints_4) is
      procedure glColor4iv (V : in Ints_4);
      pragma Import (StdCall, glColor4iv, "glColor4iv");
   begin  -- Color
      glColor4iv (V);
   end Color;

   procedure Color (V : in Floats_3) is
      procedure glColor3fv (V : in Floats_3);
      pragma Import (StdCall, glColor3fv, "glColor3fv");
   begin  -- Color
      glColor3fv (V);
   end Color;

   procedure Color (V : in Floats_4) is
      procedure glColor4fv (V : in Floats_4);
      pragma Import (StdCall, glColor4fv, "glColor4fv");
   begin  -- Color
      glColor4fv (V);
   end Color;

   procedure Color (V : in Doubles_3) is
      procedure glColor3dv (V : in Doubles_3);
      pragma Import (StdCall, glColor3dv, "glColor3dv");
   begin  -- Color
      glColor3dv (V);
   end Color;

   procedure Color (V : in Doubles_4) is
      procedure glColor4dv (V : in Doubles_4);
      pragma Import (StdCall, glColor4dv, "glColor4dv");
   begin  -- Color
      glColor4dv (V);
   end Color;

   procedure Color (V : in UBytes_3) is
      procedure glColor3ubv (V : in UBytes_3);
      pragma Import (StdCall, glColor3ubv, "glColor3ubv");
   begin  -- Color
      glColor3ubv (V);
   end Color;

   procedure Color (V : in UBytes_4) is
      procedure glColor4ubv (V : in UBytes_4);
      pragma Import (StdCall, glColor4ubv, "glColor4ubv");
   begin  -- Color
      glColor4ubv (V);
   end Color;

   procedure Color (V : in UShorts_3) is
      procedure glColor3usv (V : in UShorts_3);
      pragma Import (StdCall, glColor3usv, "glColor3usv");
   begin  -- Color
      glColor3usv (V);
   end Color;

   procedure Color (V : in UShorts_4) is
      procedure glColor4usv (V : in UShorts_4);
      pragma Import (StdCall, glColor4usv, "glColor4usv");
   begin  -- Color
      glColor4usv (V);
   end Color;

   procedure Color (V : in UInts_3) is
      procedure glColor3uiv (V : in UInts_3);
      pragma Import (StdCall, glColor3uiv, "glColor3uiv");
   begin  -- Color
      glColor3uiv (V);
   end Color;

   procedure Color (V : in UInts_4) is
      procedure glColor4uiv (V : in UInts_4);
      pragma Import (StdCall, glColor4uiv, "glColor4uiv");
   begin  -- Color
      glColor4uiv (V);
   end Color;

   ---------------------------------------------------------------------------

   -- Lighting and materials
   procedure Light (Light  : in Enum;
                    PName  : in Enum;
                    Params : in Int_Params) is
      procedure glLightiv (Light  : in Enum;
                           PName  : in Enum;
                           Params : in System.Address);
      pragma Import (StdCall, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, PName, Params (Params'First)'Address);
   end Light;

   procedure Light (Light  : in Enum;
                    PName  : in Enum;
                    Params : in Float_Params) is
      procedure glLightfv (Light  : in Enum;
                           PName  : in Enum;
                           Params : in System.Address);
      pragma Import (StdCall, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, PName, Params (Params'First)'Address);
   end Light;

   procedure Material (Face   : in Enum;
                       PName  : in Enum;
                       Params : in Int_Params) is
      procedure glMaterialiv (Face   : in Enum;
                              PName  : in Enum;
                              Params : in System.Address);
      pragma Import (StdCall, glMaterialiv, "glMaterialiv");
   begin  -- Material
      glMaterialiv (Face, PName, Params (Params'First)'Address);
   end Material;

   procedure Material (Face   : in Enum;
                       PName  : in Enum;
                       Params : in Float_Params) is
      procedure glMaterialfv (Face   : in Enum;
                              PName  : in Enum;
                              Params : in System.Address);
      pragma Import (StdCall, glMaterialfv, "glMaterialfv");
   begin  -- Material
      glMaterialfv (Face, PName, Params (Params'First)'Address);
   end Material;

   ---------------------------------------------------------------------------

   -- Lighting
   procedure Light (Light : in Enum; P_Name : in Enum; Param : in Float) is
      procedure glLightf (Light : in Enum;
                          P_Name : in Enum;
                          Param  : in Float);
      pragma Import (StdCall, glLightf, "glLightf");
   begin  -- Light
      glLightf (Light, P_Name, Param);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Floats_1) is
      procedure glLightfv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Floats_1);
      pragma Import (StdCall, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Floats_3) is
      procedure glLightfv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Floats_3);
      pragma Import (StdCall, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Floats_4) is
      procedure glLightfv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Floats_4);
      pragma Import (StdCall, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Param : Int) is
      procedure glLighti (Light : in Enum;
                          P_Name : in Enum;
                          Param  : in Int);
      pragma Import (StdCall, glLighti, "glLighti");
   begin  -- Light
      glLighti (Light, P_Name, Param);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Ints_1) is
      procedure glLightiv (Light  : in Enum;
                           P_Name : in Enum;
                           Params : in Ints_1);
      pragma Import (StdCall, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Ints_3) is
      procedure glLightiv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Ints_3);
      pragma Import (StdCall, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Ints_4) is
      procedure glLightiv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Ints_4);
      pragma Import (StdCall, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, P_Name, Params);
   end Light;

   -- Normal Vector
   procedure Normal (X, Y, Z : Byte) is
      procedure glNormal3b (X, Y, Z : Byte);
      pragma Import (StdCall, glNormal3b, "glNormal3b");
   begin  -- Normal
      glNormal3b (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Double) is
      procedure glNormal3d (X, Y, Z : Double);
      pragma Import (StdCall, glNormal3d, "glNormal3d");
   begin  -- Normal
      glNormal3d (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Float) is
      procedure glNormal3f (X, Y, Z : Float);
      pragma Import (StdCall, glNormal3f, "glNormal3f");
   begin  -- Normal
      glNormal3f (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Int) is
      procedure glNormal3i (X, Y, Z : Int);
      pragma Import (StdCall, glNormal3i, "glNormal3i");
   begin  -- Normal
      glNormal3i (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Short) is
      procedure glNormal3s (X, Y, Z : Short);
      pragma Import (StdCall, glNormal3s, "glNormal3s");
   begin  -- Normal
      glNormal3s (X, Y, Z);
   end Normal;

   procedure Normal (V : Bytes_3) is
      procedure glNormal3bv (V : Bytes_3);
      pragma Import (StdCall, glNormal3bv, "glNormal3bv");
   begin  -- Normal
      glNormal3bv (V);
   end Normal;

   procedure Normal (V : Doubles_3) is
      procedure glNormal3dv (V : Doubles_3);
      pragma Import (StdCall, glNormal3dv, "glNormal3dv");
   begin  -- Normal
      glNormal3dv (V);
   end Normal;

   procedure Normal (V : Floats_3) is
      procedure glNormal3fv (V : Floats_3);
      pragma Import (StdCall, glNormal3fv, "glNormal3fv");
   begin  -- Normal
      glNormal3fv (V);
   end Normal;

   procedure Normal (V : Ints_3) is
      procedure glNormal3iv (V : Ints_3);
      pragma Import (StdCall, glNormal3iv, "glNormal3iv");
   begin  -- Normal
      glNormal3iv (V);
   end Normal;

   procedure Normal (V : Shorts_3) is
      procedure glNormal3sv (V : Shorts_3);
      pragma Import (StdCall, glNormal3sv, "glNormal3sv");
   begin  -- Normal
      glNormal3sv (V);
   end Normal;


   procedure Tex_Gen (Coord : in Enum;
                      PName : in Enum;
                      Param : in Int) is
      procedure glTexGeni (Coord : in Enum;
                           PName : in Enum;
                           Param : in Int);
      pragma Import (StdCall, glTexGeni, "glTexGeni");
   begin  -- Tex_Gen
      glTexGeni (Coord, PName, Param);
   end Tex_Gen;

   procedure Tex_Gen (Coord : in Enum;
                      PName : in Enum;
                      Param : in Float) is
      procedure glTexGenf (Coord : in Enum;
                           PName : in Enum;
                           Param : in Float);
      pragma Import (StdCall, glTexGenf, "glTexGenf");
   begin  -- Tex_Gen
      glTexGenf (Coord, PName, Param);
   end Tex_Gen;

   procedure Tex_Gen (Coord : in Enum;
                      PName : in Enum;
                      Param : in Double) is
      procedure glTexGend (Coord : in Enum;
                           PName : in Enum;
                           Param : in Double);
      pragma Import (StdCall, glTexGend, "glTexGend");
   begin  -- Tex_Gen
      glTexGend (Coord, PName, Param);
   end Tex_Gen;

   procedure Tex_Parameter (Target : in Enum;
                           PName  : in Enum;
                           Param  : in Enum) is
      procedure glTexParameteri (Target : in Enum;
                                 PName  : in Enum;
                                 Param  : in Enum);
      pragma Import (StdCall, glTexParameteri, "glTexParameteri");
   begin  -- TexParameter
      glTexParameteri (Target, PName, Param);
   end Tex_Parameter;

   procedure Tex_Parameter (Target : in Enum;
                           PName  : in Enum;
                           Param  : in Int) is
      procedure glTexParameteri (Target : in Enum;
                                 PName  : in Enum;
                                 Param  : in Int);
      pragma Import (StdCall, glTexParameteri, "glTexParameteri");
   begin  -- TexParameter
      glTexParameteri (Target, PName, Param);
   end Tex_Parameter;

   procedure Tex_Parameter (Target : in Enum;
                           PName  : in Enum;
                           Param  : in Float) is
      procedure glTexParameterf (Target : in Enum;
                                 PName  : in Enum;
                                 Param  : in Float);
      pragma Import (StdCall, glTexParameterf, "glTexParameterf");
   begin  -- TexParameter
      glTexParameterf (Target, PName, Param);
   end Tex_Parameter;

   -- Texture images
   procedure Tex_Image (Target          : in Enum;
                       Level           : in Int;
                       Internal_Format : in Enum;
                       Width           : in SizeI;
                       Border          : in Int;
                       Format          : in Enum;
                       Pixel_Type      : in Enum;
                       Pixels          : in System.Address) is
      procedure glTexImage1D (Target          : in Enum;
                              Level           : in Int;
                              Internal_Format : in Enum;
                              Width           : in SizeI;
                              Border          : in Int;
                              Format          : in Enum;
                              Pixel_Type      : in Enum;
                              Pixels          : in System.Address);
      pragma Import (StdCall, glTexImage1D, "glTexImage1D");
   begin  -- TexImage
      glTexImage1D (Target, Level, Internal_Format, Width, Border, Format, Pixel_Type, Pixels);
   end Tex_Image;

   procedure Tex_Image (Target          : in Enum;
                       Level           : in Int;
                       Internal_Format : in Enum;
                       Width           : in SizeI;
                       Height          : in SizeI;
                       Border          : in Int;
                       Format          : in Enum;
                       Pixel_Type      : in Enum;
                       Pixels          : in System.Address) is
      procedure glTexImage2D (Target          : in Enum;
                              Level           : in Int;
                              Internal_Format : in Enum;
                              Width           : in SizeI;
                              Height          : in SizeI;
                              Border          : in Int;
                              Format          : in Enum;
                              Pixel_Type      : in Enum;
                              Pixels          : in System.Address);
      pragma Import (StdCall, glTexImage2D, "glTexImage2D");
   begin  -- TexImage
      glTexImage2D (Target, Level, Internal_Format, Width, Height, Border, Format, Pixel_Type, Pixels);
   end Tex_Image;

   procedure Tex_Sub_Image (Target      : in Enum;
                            Level      : in Int;
                            X_Offset   : in Int;
                            Y_Offset   : in Int;
                            Width      : in SizeI;
                            Height     : in SizeI;
                            Format     : in Enum;
                            Pixel_Type : in Enum;
                            Pixels     : in GL.Pointer) is
      procedure glTexSubImage2D (Target      : in Enum;
                                 Level      : in Int;
                                 X_Offset   : in Int;
                                 Y_Offset   : in Int;
                                 Width      : in SizeI;
                                 Height     : in SizeI;
                                 Format     : in Enum;
                                 Pixel_Type : in Enum;
                                 Pixels     : in GL.Pointer);
      pragma Import (StdCall, glTexSubImage2D, "glTexSubImage2D");
   begin
      glTexSubImage2D (Target, Level, X_Offset, Y_Offset, Width, Height, Format, Pixel_Type, Pixels);
   end Tex_Sub_Image;

--   procedure Tex_Image (Target          : in Enum;
--                       Level           : in Int;
--                       Internal_Format : in Int;
--                       Width           : in SizeI;
--                       Height          : in SizeI;
--                       Depth           : in SizeI;
--                       Border          : in Int;
--                       Format          : in Enum;
--                       Pixel_Type      : in Enum;
--                       Pixels          : in System.Address) is
--      procedure glTexImage3D (Target          : in Enum;
--                              Level           : in Int;
--                              Internal_Format : in Int;
--                              Width           : in SizeI;
--                              Height          : in SizeI;
--                              Depth           : in SizeI;
--                              Border          : in Int;
--                              Format          : in Enum;
--                              Pixel_Type      : in Enum;
--                              Pixels          : in System.Address);
--      pragma Import (StdCall, glTexImage3D, "glTexImage3D");
--   begin  -- TexImage
--      glTexImage3D (Target, Level, Internal_Format, Width, Height, Depth, Border, Format, Pixel_Type, Pixels);
--   end Tex_Image;

   -- Texture coordinates
   procedure Tex_Coord (S : in Short) is
      procedure glTexCoord1s (S : in Short);
      pragma Import (StdCall, glTexCoord1s, "glTexCoord1s");
   begin  -- Tex_Coord
      glTexCoord1s (S);
   end Tex_Coord;

   procedure Tex_Coord (S : in Int) is
      procedure glTexCoord1i (S : in Int);
      pragma Import (StdCall, glTexCoord1i, "glTexCoord1i");
   begin  -- Tex_Coord
      glTexCoord1i (S);
   end Tex_Coord;

   procedure Tex_Coord (S : in Float) is
      procedure glTexCoord1f (S : in Float);
      pragma Import (StdCall, glTexCoord1f, "glTexCoord1f");
   begin  -- Tex_Coord
      glTexCoord1f (S);
   end Tex_Coord;

   procedure Tex_Coord (S : in Double) is
      procedure glTexCoord1d (S : in Double);
      pragma Import (StdCall, glTexCoord1d, "glTexCoord1d");
   begin  -- Tex_Coord
      glTexCoord1d (S);
   end Tex_Coord;

   procedure Tex_Coord (S : in Short;
                       T : in Short) is
      procedure glTexCoord2s (S : in Short;
                              T : in Short);
      pragma Import (StdCall, glTexCoord2s, "glTexCoord2s");
   begin  -- Tex_Coord
      glTexCoord2s (S, T);
   end Tex_Coord;

   procedure Tex_Coord (S : in Int;
                       T : in Int) is
      procedure glTexCoord2i (S : in Int;
                              T : in Int);
      pragma Import (StdCall, glTexCoord2i, "glTexCoord2i");
   begin  -- Tex_Coord
      glTexCoord2i (S, T);
   end Tex_Coord;

   procedure Tex_Coord (S : in Float;
                       T : in Float) is
      procedure glTexCoord2f (S : in Float;
                              T : in Float);
      pragma Import (StdCall, glTexCoord2f, "glTexCoord2f");
   begin  -- Tex_Coord
      glTexCoord2f (S, T);
   end Tex_Coord;

   procedure Tex_Coord (S : in Double;
                       T : in Double) is
      procedure glTexCoord2d (S : in Double;
                              T : in Double);
      pragma Import (StdCall, glTexCoord2d, "glTexCoord2d");
   begin  -- Tex_Coord
      glTexCoord2d (S, T);
   end Tex_Coord;

   procedure Tex_Coord (S : in Short;
                       T : in Short;
                       R : in Short) is
      procedure glTexCoord3s (S : in Short;
                              T : in Short;
                              R : in Short);
      pragma Import (StdCall, glTexCoord3s, "glTexCoord3s");
   begin  -- Tex_Coord
      glTexCoord3s (S, T, R);
   end Tex_Coord;

   procedure Tex_Coord (S : in Int;
                       T : in Int;
                       R : in Int) is
      procedure glTexCoord3i (S : in Int;
                              T : in Int;
                              R : in Int);
      pragma Import (StdCall, glTexCoord3i, "glTexCoord3i");
   begin  -- Tex_Coord
      glTexCoord3i (S, T, R);
   end Tex_Coord;

   procedure Tex_Coord (S : in Float;
                       T : in Float;
                       R : in Float) is
      procedure glTexCoord3f (S : in Float;
                              T : in Float;
                              R : in Float);
      pragma Import (StdCall, glTexCoord3f, "glTexCoord3f");
   begin  -- Tex_Coord
      glTexCoord3f (S, T, R);
   end Tex_Coord;

   procedure Tex_Coord (S : in Double;
                       T : in Double;
                       R : in Double) is
      procedure glTexCoord3d (S : in Double;
                              T : in Double;
                              R : in Double);
      pragma Import (StdCall, glTexCoord3d, "glTexCoord3d");
   begin  -- Tex_Coord
      glTexCoord3d (S, T, R);
   end Tex_Coord;

   procedure Tex_Coord (S : in Short;
                       T : in Short;
                       R : in Short;
                       Q : in Short) is
      procedure glTexCoord4s (S : in Short;
                              T : in Short;
                              R : in Short;
                              Q : in Short);
      pragma Import (StdCall, glTexCoord4s, "glTexCoord4s");
   begin  -- Tex_Coord
      glTexCoord4s (S, T, R, Q);
   end Tex_Coord;

   procedure Tex_Coord (S : in Int;
                       T : in Int;
                       R : in Int;
                       Q : in Int) is
      procedure glTexCoord4i (S : in Int;
                              T : in Int;
                              R : in Int;
                              Q : in Int);
      pragma Import (StdCall, glTexCoord4i, "glTexCoord4i");
   begin  -- Tex_Coord
      glTexCoord4i (S, T, R, Q);
   end Tex_Coord;

   procedure Tex_Coord (S : in Float;
                       T : in Float;
                       R : in Float;
                       Q : in Float) is
      procedure glTexCoord4f (S : in Float;
                              T : in Float;
                              R : in Float;
                              Q : in Float);
      pragma Import (StdCall, glTexCoord4f, "glTexCoord4f");
   begin  -- Tex_Coord
      glTexCoord4f (S, T, R, Q);
   end Tex_Coord;

   procedure Tex_Coord (S : in Double;
                       T : in Double;
                       R : in Double;
                       Q : in Double) is
      procedure glTexCoord4d (S : in Double;
                              T : in Double;
                              R : in Double;
                              Q : in Double);
      pragma Import (StdCall, glTexCoord4d, "glTexCoord4d");
   begin  -- Tex_Coord
      glTexCoord4d (S, T, R, Q);
   end Tex_Coord;

   procedure Tex_Coord (V : in Shorts_1) is
      procedure glTexCoord1sv (S : in Shorts_1);
      pragma Import (StdCall, glTexCoord1sv, "glTexCoord1sv");
   begin  -- Tex_Coord
      glTexCoord1sv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Shorts_2) is
      procedure glTexCoord2sv (S : in Shorts_2);
      pragma Import (StdCall, glTexCoord2sv, "glTexCoord2sv");
   begin  -- Tex_Coord
      glTexCoord2sv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Shorts_3) is
      procedure glTexCoord3sv (S : in Shorts_3);
      pragma Import (StdCall, glTexCoord3sv, "glTexCoord3sv");
   begin  -- Tex_Coord
      glTexCoord3sv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Shorts_4) is
      procedure glTexCoord4sv (S : in Shorts_4);
      pragma Import (StdCall, glTexCoord4sv, "glTexCoord4sv");
   begin  -- Tex_Coord
      glTexCoord4sv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Ints_1) is
      procedure glTexCoord1iv (S : in Ints_1);
      pragma Import (StdCall, glTexCoord1iv, "glTexCoord1iv");
   begin  -- Tex_Coord
      glTexCoord1iv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Ints_2) is
      procedure glTexCoord2iv (S : in Ints_2);
      pragma Import (StdCall, glTexCoord2iv, "glTexCoord2iv");
   begin  -- Tex_Coord
      glTexCoord2iv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Ints_3) is
      procedure glTexCoord3iv (S : in Ints_3);
      pragma Import (StdCall, glTexCoord3iv, "glTexCoord3iv");
   begin  -- Tex_Coord
      glTexCoord3iv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Ints_4) is
      procedure glTexCoord4iv (S : in Ints_4);
      pragma Import (StdCall, glTexCoord4iv, "glTexCoord4iv");
   begin  -- Tex_Coord
      glTexCoord4iv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Floats_1) is
      procedure glTexCoord1fv (S : in Floats_1);
      pragma Import (StdCall, glTexCoord1fv, "glTexCoord1fv");
   begin  -- Tex_Coord
      glTexCoord1fv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Floats_2) is
      procedure glTexCoord2fv (S : in Floats_2);
      pragma Import (StdCall, glTexCoord2fv, "glTexCoord2fv");
   begin  -- Tex_Coord
      glTexCoord2fv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Floats_3) is
      procedure glTexCoord3fv (S : in Floats_3);
      pragma Import (StdCall, glTexCoord3fv, "glTexCoord3fv");
   begin  -- Tex_Coord
      glTexCoord3fv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Floats_4) is
      procedure glTexCoord4fv (S : in Floats_4);
      pragma Import (StdCall, glTexCoord4fv, "glTexCoord4fv");
   begin  -- Tex_Coord
      glTexCoord4fv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Doubles_1) is
      procedure glTexCoord1dv (S : in Doubles_1);
      pragma Import (StdCall, glTexCoord1dv, "glTexCoord1dv");
   begin  -- Tex_Coord
      glTexCoord1dv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Doubles_2) is
      procedure glTexCoord2dv (S : in Doubles_2);
      pragma Import (StdCall, glTexCoord2dv, "glTexCoord2dv");
   begin  -- Tex_Coord
      glTexCoord2dv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Doubles_3) is
      procedure glTexCoord3dv (S : in Doubles_3);
      pragma Import (StdCall, glTexCoord3dv, "glTexCoord3dv");
   begin  -- Tex_Coord
      glTexCoord3dv (V);
   end Tex_Coord;

   procedure Tex_Coord (V : in Doubles_4) is
      procedure glTexCoord4dv (S : in Doubles_4);
      pragma Import (StdCall, glTexCoord4dv, "glTexCoord4dv");
   begin  -- Tex_Coord
      glTexCoord4dv (V);
   end Tex_Coord;

   ---------------------------------------------------------------------------

   procedure Map (Target : in Enum;
                  U1     : in Float;
                  U2     : in Float;
                  Stride : in Int;
                  Order  : in Int;
                  Points : in System.Address) is
      procedure glMap1f (Target : in Enum;
                         U1     : in Float;
                         U2     : in Float;
                         Stride : in Int;
                         Order  : in Int;
                         Points : in System.Address);
      pragma Import (StdCall, glMap1f, "glMap1f");
   begin  -- Map
      glMap1f (Target, U1, U2, Stride, Order, Points);
   end Map;

   procedure Map (Target : in Enum;
                  U1     : in Double;
                  U2     : in Double;
                  Stride : in Int;
                  Order  : in Int;
                  Points : in System.Address) is
      procedure glMap1d (Target : in Enum;
                         U1     : in Double;
                         U2     : in Double;
                         Stride : in Int;
                         Order  : in Int;
                         Points : in System.Address);
      pragma Import (StdCall, glMap1d, "glMap1d");
   begin  -- Map
      glMap1d (Target, U1, U2, Stride, Order, Points);
   end Map;

   procedure Map (Target  : in Enum;
                  U1      : in Float;
                  U2      : in Float;
                  UStride : in Int;
                  UOrder  : in Int;
                  V1      : in Float;
                  V2      : in Float;
                  VStride : in Int;
                  VOrder  : in Int;
                  Points  : in System.Address) is
      procedure glMap2f (Target  : in Enum;
                         U1      : in Float;
                         U2      : in Float;
                         UStride : in Int;
                         UOrder  : in Int;
                         V1      : in Float;
                         V2      : in Float;
                         VStride : in Int;
                         VOrder  : in Int;
                         Points : in System.Address);
      pragma Import (StdCall, glMap2f, "glMap2f");
   begin  -- Map
      glMap2f (Target, U1, U2, UStride, UOrder, V1, V2, VStride, VOrder, Points);
   end Map;

   procedure Map (Target  : in Enum;
                  U1      : in Double;
                  U2      : in Double;
                  UStride : in Int;
                  UOrder  : in Int;
                  V1      : in Double;
                  V2      : in Double;
                  VStride : in Int;
                  VOrder  : in Int;
                  Points  : in System.Address) is
      procedure glMap2d (Target  : in Enum;
                         U1      : in Double;
                         U2      : in Double;
                         UStride : in Int;
                         UOrder  : in Int;
                         V1      : in Double;
                         V2      : in Double;
                         VStride : in Int;
                         VOrder  : in Int;
                         Points : in System.Address);
      pragma Import (StdCall, glMap2d, "glMap2d");
   begin  -- Map
      glMap2d (Target, U1, U2, UStride, UOrder, V1, V2, VStride, VOrder, Points);
   end Map;

   procedure Map_Grid (Un : in Int;
                       U1 : in Float;
                       U2 : in Float) is
      procedure glMapGrid1f (Un : in Int;
                             U1 : in Float;
                             U2 : in Float);
      pragma Import (StdCall, glMapGrid1f, "glMapGrid1f");
   begin  -- Map_Grid
      glMapGrid1f (Un, U1, U2);
   end Map_Grid;

   procedure Map_Grid (Un : in Int;
                       U1 : in Double;
                       U2 : in Double) is
      procedure glMapGrid1d (Un : in Int;
                             U1 : in Double;
                             U2 : in Double);
      pragma Import (StdCall, glMapGrid1d, "glMapGrid1d");
   begin  -- Map_Grid
      glMapGrid1d (Un, U1, U2);
   end Map_Grid;

   procedure Map_Grid (Un : in Int;
                       U1 : in Float;
                       U2 : in Float;
                       Vn : in Int;
                       V1 : in Float;
                       V2 : in Float) is
      procedure glMapGrid2f (Un : in Int;
                             U1 : in Float;
                             U2 : in Float;
                             Vn : in Int;
                             V1 : in Float;
                             V2 : in Float);
      pragma Import (StdCall, glMapGrid2f, "glMapGrid2f");
   begin  -- Map_Grid
      glMapGrid2f (Un, U1, U2, Vn, V1, V2);
   end Map_Grid;

   procedure Map_Grid (Un : in Int;
                       U1 : in Double;
                       U2 : in Double;
                       Vn : in Int;
                       V1 : in Double;
                       V2 : in Double) is
      procedure glMapGrid2d (Un : in Int;
                             U1 : in Double;
                             U2 : in Double;
                             Vn : in Int;
                             V1 : in Double;
                             V2 : in Double);
      pragma Import (StdCall, glMapGrid2d, "glMapGrid2d");
   begin  -- Map_Grid
      glMapGrid2d (Un, U1, U2, Vn, V1, V2);
   end Map_Grid;

   procedure Eval_Point (I : Int) is
      procedure glEvalPoint1 (I : in Int);
      pragma Import (StdCall, glEvalPoint1, "glEvalPoint1");
   begin  -- Eval_Point
      glEvalPoint1 (I);
   end Eval_Point;

   procedure Eval_Point (I : in Int;
                         J : in Int) is
      procedure glEvalPoint2 (I : in Int;
                              J : in Int);
      pragma Import (StdCall, glEvalPoint2, "glEvalPoint2");
   begin  -- Eval_Point
      glEvalPoint2 (I, J);
   end Eval_Point;

   procedure Eval_Mesh (Mode : in Enum;
                        I1   : in Int;
                        I2   : in Int) is
      procedure glEvalMesh1 (Mode : in Enum;
                             I1   : in Int;
                             I2   : in Int);
      pragma Import (StdCall, glEvalMesh1, "glEvalMesh1");
   begin  -- Eval_Mesh
      glEvalMesh1 (Mode, I1, I2);
   end Eval_Mesh;

   procedure Eval_Mesh (Mode : in Enum;
                        I1   : in Int;
                        I2   : in Int;
                        J1   : in Int;
                        J2   : in Int) is
      procedure glEvalMesh2 (Mode : in Enum;
                             I1   : in Int;
                             I2   : in Int;
                             J1   : in Int;
                             J2   : in Int);
      pragma Import (StdCall, glEvalMesh2, "glEvalMesh2");
   begin  -- Eval_Mesh
      glEvalMesh2 (Mode, I1, I2, J1, J2);
   end Eval_Mesh;

   ---------------------------------------------------------------------------

   procedure Vertex (X : in Short;
                     Y : in Short) is
      procedure glVertex2s (X : in Short;
                            Y : in Short);
      pragma Import (StdCall, glVertex2s, "glVertex2s");
   begin  -- Vertex
      glVertex2s (X, Y);
   end Vertex;

   procedure Vertex (X : in Int;
                     Y : in Int) is
      procedure glVertex2i (X : in Int;
                            Y : in Int);
      pragma Import (StdCall, glVertex2i, "glVertex2i");
   begin  -- Vertex
      glVertex2i (X, Y);
   end Vertex;

   procedure Vertex (X : in Float;
                     Y : in Float) is
      procedure glVertex2f (X : in Float;
                            Y : in Float);
      pragma Import (StdCall, glVertex2f, "glVertex2f");
   begin  -- Vertex
      glVertex2f (X, Y);
   end Vertex;

   procedure Vertex (X : in Double;
                     Y : in Double) is
      procedure glVertex2d (X : in Double;
                            Y : in Double);
      pragma Import (StdCall, glVertex2d, "glVertex2d");
   begin  -- Vertex
      glVertex2d (X, Y);
   end Vertex;

   procedure Vertex (X : in Short;
                     Y : in Short;
                     Z : in Short) is
      procedure glVertex3s (X : in Short;
                            Y : in Short;
                            Z : in Short);
      pragma Import (StdCall, glVertex3s, "glVertex3s");
   begin  -- Vertex
      glVertex3s (X, Y, Z);
   end Vertex;

   procedure Vertex (X : in Int;
                     Y : in Int;
                     Z : in Int) is
      procedure glVertex3i (X : in Int;
                            Y : in Int;
                            Z : in Int);
      pragma Import (StdCall, glVertex3i, "glVertex3i");
   begin  -- Vertex
      glVertex3i (X, Y, Z);
   end Vertex;

   procedure Vertex (X : in Float;
                     Y : in Float;
                     Z : in Float) is
      procedure glVertex3f (X : in Float;
                            Y : in Float;
                            Z : in Float);
      pragma Import (StdCall, glVertex3f, "glVertex3f");
   begin  -- Vertex
      glVertex3f (X, Y, Z);
   end Vertex;

   procedure Vertex (X : in Double;
                     Y : in Double;
                     Z : in Double) is
      procedure glVertex3d (X : in Double;
                            Y : in Double;
                            Z : in Double);
      pragma Import (StdCall, glVertex3d, "glVertex3d");
   begin  -- Vertex
      glVertex3d (X, Y, Z);
   end Vertex;

   procedure Vertex (X : in Short;
                     Y : in Short;
                     Z : in Short;
                     W : in Short) is
      procedure glVertex4s (X : in Short;
                            Y : in Short;
                            Z : in Short;
                            W : in Short);
      pragma Import (StdCall, glVertex4s, "glVertex4s");
   begin  -- Vertex
      glVertex4s (X, Y, Z, W);
   end Vertex;

   procedure Vertex (X : in Int;
                     Y : in Int;
                     Z : in Int;
                     W : in Int) is
      procedure glVertex4i (X : in Int;
                            Y : in Int;
                            Z : in Int;
                            W : in Int);
      pragma Import (StdCall, glVertex4i, "glVertex4i");
   begin  -- Vertex
      glVertex4i (X, Y, Z, W);
   end Vertex;

   procedure Vertex (X : in Float;
                     Y : in Float;
                     Z : in Float;
                     W : in Float) is
      procedure glVertex4f (X : in Float;
                            Y : in Float;
                            Z : in Float;
                            W : in Float);
      pragma Import (StdCall, glVertex4f, "glVertex4f");
   begin  -- Vertex
      glVertex4f (X, Y, Z, W);
   end Vertex;

   procedure Vertex (X : in Double;
                     Y : in Double;
                     Z : in Double;
                     W : in Double) is
      procedure glVertex4d (X : in Double;
                            Y : in Double;
                            Z : in Double;
                            W : in Double);
      pragma Import (StdCall, glVertex4d, "glVertex4d");
   begin  -- Vertex
      glVertex4d (X, Y, Z, W);
   end Vertex;

   procedure Vertex (V : in Shorts_2) is
      procedure glVertex2sv (V : in Shorts_2);
      pragma Import (StdCall, glVertex2sv, "glVertex2sv");
   begin  -- Vertex
      glVertex2sv (V);
   end Vertex;

   procedure Vertex (V : in Shorts_3) is
      procedure glVertex3sv (V : in Shorts_3);
      pragma Import (StdCall, glVertex3sv, "glVertex3sv");
   begin  -- Vertex
      glVertex3sv (V);
   end Vertex;

   procedure Vertex (V : in Shorts_4) is
      procedure glVertex4sv (V : in Shorts_4);
      pragma Import (StdCall, glVertex4sv, "glVertex4sv");
   begin  -- Vertex
      glVertex4sv (V);
   end Vertex;

   procedure Vertex (V : in Ints_2) is
      procedure glVertex2iv (V : in Ints_2);
      pragma Import (StdCall, glVertex2iv, "glVertex2iv");
   begin  -- Vertex
      glVertex2iv (V);
   end Vertex;

   procedure Vertex (V : in Ints_3) is
      procedure glVertex3iv (V : in Ints_3);
      pragma Import (StdCall, glVertex3iv, "glVertex3iv");
   begin  -- Vertex
      glVertex3iv (V);
   end Vertex;

   procedure Vertex (V : in Ints_4) is
      procedure glVertex4iv (V : in Ints_4);
      pragma Import (StdCall, glVertex4iv, "glVertex4iv");
   begin  -- Vertex
      glVertex4iv (V);
   end Vertex;

   procedure Vertex (V : in Floats_2) is
      procedure glVertex2fv (V : in Floats_2);
      pragma Import (StdCall, glVertex2fv, "glVertex2fv");
   begin  -- Vertex
      glVertex2fv (V);
   end Vertex;

   procedure Vertex (V : in Floats_3) is
      procedure glVertex3fv (V : in Floats_3);
      pragma Import (StdCall, glVertex3fv, "glVertex3fv");
   begin  -- Vertex
      glVertex3fv (V);
   end Vertex;

   procedure Vertex (V : in Floats_4) is
      procedure glVertex4fv (V : in Floats_4);
      pragma Import (StdCall, glVertex4fv, "glVertex4fv");
   begin  -- Vertex
      glVertex4fv (V);
   end Vertex;

   procedure Vertex (V : in Doubles_2) is
      procedure glVertex2dv (V : in Doubles_2);
      pragma Import (StdCall, glVertex2dv, "glVertex2dv");
   begin  -- Vertex
      glVertex2dv (V);
   end Vertex;

   procedure Vertex (V : in Doubles_3) is
      procedure glVertex3dv (V : in Doubles_3);
      pragma Import (StdCall, glVertex3dv, "glVertex3dv");
   begin  -- Vertex
      glVertex3dv (V);
   end Vertex;

   procedure Vertex (V : in Doubles_4) is
      procedure glVertex4dv (V : in Doubles_4);
      pragma Import (StdCall, glVertex4dv, "glVertex4dv");
   begin  -- Vertex
      glVertex4dv (V);
   end Vertex;

   ---------------------------------------------------------------------------

   procedure Vertex_Pointer (Size : in SizeI;
                             Element_Type : in Enum;
                             Stride : in SizeI;
                             Data_Pointer : in Pointer) is
      procedure glVertexPointer (Size : in SizeI;
                                 Element_Type : in Enum;
                                 Stride : in SizeI;
                                 Data_Pointer : in System.Address);
      pragma Import (StdCall, glVertexPointer, "glVertexPointer");
   begin
      glVertexPointer (Size, Element_Type, Stride, Data_Pointer);
   end Vertex_Pointer;

   procedure Vertex_Pointer (Size : in SizeI;
                             Element_Type : in Enum;
                             Stride : in SizeI;
                             Offset : in SizeI) is
      procedure glVertexPointer (Size : in SizeI;
                                 Element_Type : in Enum;
                                 Stride : in SizeI;
                                 Offset : in SizeI);
      pragma Import (StdCall, glVertexPointer, "glVertexPointer");
   begin
      glVertexPointer (Size, Element_Type, Stride, Offset);
   end Vertex_Pointer;

   ---------------------------------------------------------------------------

   -- Shader variables
   function Get_Uniform_Location (Program : UInt;   Name : String) return Int is
      C_Name : Interfaces.C.char_array := Interfaces.C.To_C (Name);

      function glGetUniformLocation (Program : UInt;   Name : Pointer) return Int;
      pragma Import (StdCall, glGetUniformLocation, "glGetUniformLocation");

   begin  -- Get_Uniform_Location
      return glGetUniformLocation (Program, C_Name'Address);
   end Get_Uniform_Location;

   procedure Uniform (Location : in Int;
                      V0       : in Float) is
      procedure glUniform1f (Location : in Int;
                             V0       : in Float);
      pragma Import (StdCall, glUniform1f, "glUniform1f");
   begin  -- Uniform
      glUniform1f (Location, V0);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in Float;
                      V1       : in Float) is
      procedure glUniform2f (Location : in Int;
                             V0       : in Float;
                             V1       : in Float);
      pragma Import (StdCall, glUniform2f, "glUniform2f");
   begin  -- Uniform
      glUniform2f (Location, V0, V1);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in Float;
                      V1       : in Float;
                      V2       : in Float) is
      procedure glUniform3f (Location : in Int;
                             V0       : in Float;
                             V1       : in Float;
                             V2       : in Float);
      pragma Import (StdCall, glUniform3f, "glUniform3f");
   begin  -- Uniform
      glUniform3f (Location, V0, V1, V2);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in Float;
                      V1       : in Float;
                      V2       : in Float;
                      V3       : in Float) is
      procedure glUniform4f (Location : in Int;
                             V0       : in Float;
                             V1       : in Float;
                             V2       : in Float;
                             V3       : in Float);
      pragma Import (StdCall, glUniform4f, "glUniform4f");
   begin  -- Uniform
      glUniform4f (Location, V0, V1, V2, V3);
   end Uniform;


   procedure Uniform (Location : in Int;
                      V0       : in Int) is
      procedure glUniform1i (Location : in Int;
                             V0       : in Int);
      pragma Import (StdCall, glUniform1i, "glUniform1i");
   begin  -- Uniform
      glUniform1i (Location, V0);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in Int;
                      V1       : in Int) is
      procedure glUniform2i (Location : in Int;
                             V0       : in Int;
                             V1       : in Int);
      pragma Import (StdCall, glUniform2i, "glUniform2i");
   begin  -- Uniform
      glUniform2i (Location, V0, V1);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in Int;
                      V1       : in Int;
                      V2       : in Int) is
      procedure glUniform3i (Location : in Int;
                             V0       : in Int;
                             V1       : in Int;
                             V2       : in Int);
      pragma Import (StdCall, glUniform3i, "glUniform3i");
   begin  -- Uniform
      glUniform3i (Location, V0, V1, V2);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in Int;
                      V1       : in Int;
                      V2       : in Int;
                      V3       : in Int) is
      procedure glUniform4i (Location : in Int;
                             V0       : in Int;
                             V1       : in Int;
                             V2       : in Int;
                             V3       : in Int);
      pragma Import (StdCall, glUniform4i, "glUniform4i");
   begin  -- Uniform
      glUniform4i (Location, V0, V1, V2, V3);
   end Uniform;


   procedure Uniform (Location : in Int;
                      V0       : in UInt) is
      procedure glUniform1ui (Location : in Int;
                              V0       : in UInt);
      pragma Import (StdCall, glUniform1ui, "glUniform1ui");
   begin  -- Uniform
      glUniform1ui (Location, V0);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in UInt;
                      V1       : in UInt) is
      procedure glUniform2ui (Location : in Int;
                              V0       : in UInt;
                              V1       : in UInt);
      pragma Import (StdCall, glUniform2ui, "glUniform2ui");
   begin  -- Uniform
      glUniform2ui (Location, V0, V1);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in UInt;
                      V1       : in UInt;
                      V2       : in UInt) is
      procedure glUniform3ui (Location : in Int;
                              V0       : in UInt;
                              V1       : in UInt;
                              V2       : in UInt);
      pragma Import (StdCall, glUniform3ui, "glUniform3ui");
   begin  -- Uniform
      glUniform3ui (Location, V0, V1, V2);
   end Uniform;

   procedure Uniform (Location : in Int;
                      V0       : in UInt;
                      V1       : in UInt;
                      V2       : in UInt;
                      V3       : in UInt) is
      procedure glUniform4ui (Location : in Int;
                              V0       : in UInt;
                              V1       : in UInt;
                              V2       : in UInt;
                              V3       : in UInt);
      pragma Import (StdCall, glUniform4ui, "glUniform4ui");
   begin  -- Uniform
      glUniform4ui (Location, V0, V1, V2, V3);
   end Uniform;

   function Get_Attribute_Location (Program : UInt;   Name : String) return Int is
      C_Name : Interfaces.C.char_array := Interfaces.C.To_C (Name);

      function glGetAttribLocation (Program : UInt;   Name : Pointer) return Int;
      pragma Import (StdCall, glGetAttribLocation, "glGetAttribLocation");

   begin  -- Get_Attribute_Location
      return glGetAttribLocation (Program, C_Name'Address);
   end Get_Attribute_Location;

   procedure Vertex_Attrib (Index : in UInt;
                            X     : in Float) is
      procedure glVertexAttrib1f (Index : in UInt;
                                  X     : in Float);
      Pragma Import (StdCall, glVertexAttrib1f, "glVertexAttrib1f");
   begin
      glVertexAttrib1f (Index, X);
   end Vertex_Attrib;

   procedure Vertex_Attrib (Index : in UInt;
                            X     : in Float;
                            Y     : in Float) is
      procedure glVertexAttrib2f (Index : in UInt;
                                  X     : in Float;
                                  Y     : in Float);
      Pragma Import (StdCall, glVertexAttrib2f, "glVertexAttrib2f");
   begin
      glVertexAttrib2f (Index, X, Y);
   end Vertex_Attrib;

   procedure Vertex_Attrib (Index : in UInt;
                            X     : in Float;
                            Y     : in Float;
                            Z     : in Float) is
      procedure glVertexAttrib3f (Index : in UInt;
                                  X     : in Float;
                                  Y     : in Float;
                                  Z     : in Float);
      Pragma Import (StdCall, glVertexAttrib3f, "glVertexAttrib3f");
   begin
      glVertexAttrib3f (Index, X, Y, Z);
   end Vertex_Attrib;

   procedure Vertex_Attrib (Index : in UInt;
                            X     : in Float;
                            Y     : in Float;
                            Z     : in Float;
                            W     : in Float) is
      procedure glVertexAttrib4f (Index : in UInt;
                                  X     : in Float;
                                  Y     : in Float;
                                  Z     : in Float;
                                  W     : in Float);
      Pragma Import (StdCall, glVertexAttrib4f, "glVertexAttrib4f");
   begin
      glVertexAttrib4f (Index, X, Y, Z, W);
   end  Vertex_Attrib;

   ---------------------------------------------------------------------------

end Lumen.GL;
