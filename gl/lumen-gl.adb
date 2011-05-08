
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


package body Lumen.GL is

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
      pragma Import (C, glRotated, "glRotated");
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
      pragma Import (C, glRotatef, "glRotatef");
   begin  -- Rotate
      glRotatef (Angle, X, Y, Z);
   end Rotate;

   procedure Scale (X : in Double;
                    Y : in Double;
                    Z : in Double) is
      procedure glScaled (X : in Double;
                          Y : in Double;
                          Z : in Double);
      pragma Import (C, glScaled, "glScaled");
   begin  -- Scale
      glScaled (X, Y, Z);
   end Scale;

   procedure Scale (X : in Float;
                    Y : in Float;
                    Z : in Float) is
      procedure glScalef (X : in Float;
                          Y : in Float;
                          Z : in Float);
      pragma Import (C, glScalef, "glScalef");
   begin  -- Scale
      glScalef (X, Y, Z);
   end Scale;

   procedure Translate (X : in Double;
                        Y : in Double;
                        Z : in Double) is
      procedure glTranslated (X : in Double;
                              Y : in Double;
                              Z : in Double);
      pragma Import (C, glTranslated, "glTranslated");
   begin  -- Translate
      glTranslated (X, Y, Z);
   end Translate;

   procedure Translate (X : in Float;
                        Y : in Float;
                        Z : in Float) is
      procedure glTranslatef (X : in Float;
                              Y : in Float;
                              Z : in Float);
      pragma Import (C, glTranslatef, "glTranslatef");
   begin  -- Translate
      glTranslatef (X, Y, Z);
   end Translate;

   ---------------------------------------------------------------------------

   -- Matrix operations
   procedure LoadMatrix (M : in Float_Matrix) is
      procedure glLoadMatrixf (M : in System.Address);
      pragma Import (C, glLoadMatrixf, "glLoadMatrixf");
   begin  -- LoadMatrix
      glLoadMatrixf (M'Address);
   end LoadMatrix;

   procedure LoadMatrix (M : in Double_Matrix) is
      procedure glLoadMatrixd (M : in System.Address);
      pragma Import (C, glLoadMatrixd, "glLoadMatrixd");
   begin  -- LoadMatrix
      glLoadMatrixd (M'Address);
   end LoadMatrix;

   procedure MultMatrix (M : in Float_Matrix) is
      procedure glMultMatrixf (M : in System.Address);
      pragma Import (C, glMultMatrixf, "glMultMatrixf");
   begin  -- MultMatrix
      glMultMatrixf (M'Address);
   end MultMatrix;

   procedure MultMatrix (M : in Double_Matrix) is
      procedure glMultMatrixd (M : in System.Address);
      pragma Import (C, glMultMatrixd, "glMultMatrixd");
   begin  -- MultMatrix
      glMultMatrixd (M'Address);
   end MultMatrix;

   ---------------------------------------------------------------------------

   -- Component color
   procedure Color (Red   : in Byte;
                    Green : in Byte;
                    Blue  : in Byte) is
      procedure glColor3b (Red   : in Byte;
                           Green : in Byte;
                           Blue  : in Byte);
      pragma Import (C, glColor3b, "glColor3b");
   begin  -- Color
      glColor3b (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Short;
                    Green : in Short;
                    Blue  : in Short) is
      procedure glColor3s (Red   : in Short;
                           Green : in Short;
                           Blue  : in Short);
      pragma Import (C, glColor3s, "glColor3s");
   begin  -- Color
      glColor3s (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Int;
                    Green : in Int;
                    Blue  : in Int) is
      procedure glColor3i (Red   : in Int;
                           Green : in Int;
                           Blue  : in Int);
      pragma Import (C, glColor3i, "glColor3i");
   begin  -- Color
      glColor3i (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Float;
                    Green : in Float;
                    Blue  : in Float) is
      procedure glColor3f (Red   : in Float;
                           Green : in Float;
                           Blue  : in Float);
      pragma Import (C, glColor3f, "glColor3f");
   begin  -- Color
      glColor3f (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in Double;
                    Green : in Double;
                    Blue  : in Double) is
      procedure glColor3d (Red   : in Double;
                           Green : in Double;
                           Blue  : in Double);
      pragma Import (C, glColor3d, "glColor3d");
   begin  -- Color
      glColor3d (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in UByte;
                    Green : in UByte;
                    Blue  : in UByte) is
      procedure glColor3ub (Red   : in UByte;
                            Green : in UByte;
                            Blue  : in UByte);
      pragma Import (C, glColor3ub, "glColor3ub");
   begin  -- Color
      glColor3ub (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in UShort;
                    Green : in UShort;
                    Blue  : in UShort) is
      procedure glColor3us (Red   : in UShort;
                            Green : in UShort;
                            Blue  : in UShort);
      pragma Import (C, glColor3us, "glColor3us");
   begin  -- Color
      glColor3us (Red, Green, Blue);
   end Color;

   procedure Color (Red   : in UInt;
                    Green : in UInt;
                    Blue  : in UInt) is
      procedure glColor3ui (Red   : in UInt;
                            Green : in UInt;
                            Blue  : in UInt);
      pragma Import (C, glColor3ui, "glColor3ui");
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
      pragma Import (C, glColor4b, "glColor4b");
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
      pragma Import (C, glColor4s, "glColor4s");
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
      pragma Import (C, glColor4i, "glColor4i");
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
      pragma Import (C, glColor4f, "glColor4f");
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
      pragma Import (C, glColor4d, "glColor4d");
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
      pragma Import (C, glColor4ub, "glColor4ub");
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
      pragma Import (C, glColor4us, "glColor4us");
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
      pragma Import (C, glColor4ui, "glColor4ui");
   begin  -- Color
      glColor4ui (Red, Green, Blue, Alpha);
   end Color;

   procedure Color (V : in Bytes_3) is
      procedure glColor3bv (V : in Bytes_3);
      pragma Import (C, glColor3bv, "glColor3bv");
   begin  -- Color
      glColor3bv (V);
   end Color;

   procedure Color (V : in Bytes_4) is
      procedure glColor4bv (V : in Bytes_4);
      pragma Import (C, glColor4bv, "glColor4bv");
   begin  -- Color
      glColor4bv (V);
   end Color;

   procedure Color (V : in Shorts_3) is
      procedure glColor3sv (V : in Shorts_3);
      pragma Import (C, glColor3sv, "glColor3sv");
   begin  -- Color
      glColor3sv (V);
   end Color;

   procedure Color (V : in Shorts_4) is
      procedure glColor4sv (V : in Shorts_4);
      pragma Import (C, glColor4sv, "glColor4sv");
   begin  -- Color
      glColor4sv (V);
   end Color;

   procedure Color (V : in Ints_3) is
      procedure glColor3iv (V : in Ints_3);
      pragma Import (C, glColor3iv, "glColor3iv");
   begin  -- Color
      glColor3iv (V);
   end Color;

   procedure Color (V : in Ints_4) is
      procedure glColor4iv (V : in Ints_4);
      pragma Import (C, glColor4iv, "glColor4iv");
   begin  -- Color
      glColor4iv (V);
   end Color;

   procedure Color (V : in Floats_3) is
      procedure glColor3fv (V : in Floats_3);
      pragma Import (C, glColor3fv, "glColor3fv");
   begin  -- Color
      glColor3fv (V);
   end Color;

   procedure Color (V : in Floats_4) is
      procedure glColor4fv (V : in Floats_4);
      pragma Import (C, glColor4fv, "glColor4fv");
   begin  -- Color
      glColor4fv (V);
   end Color;

   procedure Color (V : in Doubles_3) is
      procedure glColor3dv (V : in Doubles_3);
      pragma Import (C, glColor3dv, "glColor3dv");
   begin  -- Color
      glColor3dv (V);
   end Color;

   procedure Color (V : in Doubles_4) is
      procedure glColor4dv (V : in Doubles_4);
      pragma Import (C, glColor4dv, "glColor4dv");
   begin  -- Color
      glColor4dv (V);
   end Color;

   procedure Color (V : in UBytes_3) is
      procedure glColor3ubv (V : in UBytes_3);
      pragma Import (C, glColor3ubv, "glColor3ubv");
   begin  -- Color
      glColor3ubv (V);
   end Color;

   procedure Color (V : in UBytes_4) is
      procedure glColor4ubv (V : in UBytes_4);
      pragma Import (C, glColor4ubv, "glColor4ubv");
   begin  -- Color
      glColor4ubv (V);
   end Color;

   procedure Color (V : in UShorts_3) is
      procedure glColor3usv (V : in UShorts_3);
      pragma Import (C, glColor3usv, "glColor3usv");
   begin  -- Color
      glColor3usv (V);
   end Color;

   procedure Color (V : in UShorts_4) is
      procedure glColor4usv (V : in UShorts_4);
      pragma Import (C, glColor4usv, "glColor4usv");
   begin  -- Color
      glColor4usv (V);
   end Color;

   procedure Color (V : in UInts_3) is
      procedure glColor3uiv (V : in UInts_3);
      pragma Import (C, glColor3uiv, "glColor3uiv");
   begin  -- Color
      glColor3uiv (V);
   end Color;

   procedure Color (V : in UInts_4) is
      procedure glColor4uiv (V : in UInts_4);
      pragma Import (C, glColor4uiv, "glColor4uiv");
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
      pragma Import (C, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, PName, Params (Params'First)'Address);
   end Light;

   procedure Light (Light  : in Enum;
                    PName  : in Enum;
                    Params : in Float_Params) is
      procedure glLightfv (Light  : in Enum;
                           PName  : in Enum;
                           Params : in System.Address);
      pragma Import (C, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, PName, Params (Params'First)'Address);
   end Light;

   procedure Material (Face   : in Enum;
                       PName  : in Enum;
                       Params : in Int_Params) is
      procedure glMaterialiv (Face   : in Enum;
                              PName  : in Enum;
                              Params : in System.Address);
      pragma Import (C, glMaterialiv, "glMaterialiv");
   begin  -- Material
      glMaterialiv (Face, PName, Params (Params'First)'Address);
   end Material;

   procedure Material (Face   : in Enum;
                       PName  : in Enum;
                       Params : in Float_Params) is
      procedure glMaterialfv (Face   : in Enum;
                              PName  : in Enum;
                              Params : in System.Address);
      pragma Import (C, glMaterialfv, "glMaterialfv");
   begin  -- Material
      glMaterialfv (Face, PName, Params (Params'First)'Address);
   end Material;

   ---------------------------------------------------------------------------

   -- Lighting
   procedure Light (Light : in Enum; P_Name : in Enum; Param : in Float) is
      procedure glLightf (Light : in Enum;
                          P_Name : in Enum;
                          Param  : in Float);
      pragma Import (C, glLightf, "glLightf");
   begin  -- Light
      glLightf (Light, P_Name, Param);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Floats_1) is
      procedure glLightfv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Floats_1);
      pragma Import (C, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Floats_3) is
      procedure glLightfv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Floats_3);
      pragma Import (C, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Floats_4) is
      procedure glLightfv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Floats_4);
      pragma Import (C, glLightfv, "glLightfv");
   begin  -- Light
      glLightfv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Param : Int) is
      procedure glLighti (Light : in Enum;
                          P_Name : in Enum;
                          Param  : in Int);
      pragma Import (C, glLighti, "glLighti");
   begin  -- Light
      glLighti (Light, P_Name, Param);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Ints_1) is
      procedure glLightiv (Light  : in Enum;
                           P_Name : in Enum;
                           Params : in Ints_1);
      pragma Import (C, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Ints_3) is
      procedure glLightiv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Ints_3);
      pragma Import (C, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, P_Name, Params);
   end Light;

   procedure Light (Light : Enum; P_Name : Enum; Params : Ints_4) is
      procedure glLightiv (Light : in Enum;
                          P_Name : in Enum;
                          Params : in Ints_4);
      pragma Import (C, glLightiv, "glLightiv");
   begin  -- Light
      glLightiv (Light, P_Name, Params);
   end Light;

   -- Normal Vector
   procedure Normal (X, Y, Z : Byte) is
      procedure glNormal3b (X, Y, Z : Byte);
      pragma Import (C, glNormal3b, "glNormal3b");
   begin  -- Normal
      glNormal3b (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Double) is
      procedure glNormal3d (X, Y, Z : Double);
      pragma Import (C, glNormal3d, "glNormal3d");
   begin  -- Normal
      glNormal3d (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Float) is
      procedure glNormal3f (X, Y, Z : Float);
      pragma Import (C, glNormal3f, "glNormal3f");
   begin  -- Normal
      glNormal3f (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Int) is
      procedure glNormal3i (X, Y, Z : Int);
      pragma Import (C, glNormal3i, "glNormal3i");
   begin  -- Normal
      glNormal3i (X, Y, Z);
   end Normal;

   procedure Normal (X, Y, Z : Short) is
      procedure glNormal3s (X, Y, Z : Short);
      pragma Import (C, glNormal3s, "glNormal3s");
   begin  -- Normal
      glNormal3s (X, Y, Z);
   end Normal;

   procedure Normal (V : Bytes_3) is
      procedure glNormal3bv (V : Bytes_3);
      pragma Import (C, glNormal3bv, "glNormal3bv");
   begin  -- Normal
      glNormal3bv (V);
   end Normal;

   procedure Normal (V : Doubles_3) is
      procedure glNormal3dv (V : Doubles_3);
      pragma Import (C, glNormal3dv, "glNormal3dv");
   begin  -- Normal
      glNormal3dv (V);
   end Normal;

   procedure Normal (V : Floats_3) is
      procedure glNormal3fv (V : Floats_3);
      pragma Import (C, glNormal3fv, "glNormal3fv");
   begin  -- Normal
      glNormal3fv (V);
   end Normal;

   procedure Normal (V : Ints_3) is
      procedure glNormal3iv (V : Ints_3);
      pragma Import (C, glNormal3iv, "glNormal3iv");
   begin  -- Normal
      glNormal3iv (V);
   end Normal;

   procedure Normal (V : Shorts_3) is
      procedure glNormal3sv (V : Shorts_3);
      pragma Import (C, glNormal3sv, "glNormal3sv");
   begin  -- Normal
      glNormal3sv (V);
   end Normal;

   -- Texturing
   procedure TexParameter (Target : in Enum;
                           PName  : in Enum;
                           Param  : in Enum) is
      procedure glTexParameteri (Target : in Enum;
                                 PName  : in Enum;
                                 Param  : in Enum);
      pragma Import (C, glTexParameteri, "glTexParameteri");
   begin  -- TexParameter
      glTexParameteri (Target, PName, Param);
   end TexParameter;

   procedure TexParameter (Target : in Enum;
                           PName  : in Enum;
                           Param  : in Int) is
      procedure glTexParameteri (Target : in Enum;
                                 PName  : in Enum;
                                 Param  : in Int);
      pragma Import (C, glTexParameteri, "glTexParameteri");
   begin  -- TexParameter
      glTexParameteri (Target, PName, Param);
   end TexParameter;

   procedure TexParameter (Target : in Enum;
                           PName  : in Enum;
                           Param  : in Float) is
      procedure glTexParameterf (Target : in Enum;
                                 PName  : in Enum;
                                 Param  : in Float);
      pragma Import (C, glTexParameterf, "glTexParameterf");
   begin  -- TexParameter
      glTexParameterf (Target, PName, Param);
   end TexParameter;

   -- Texture images
   procedure TexImage (Target          : in Enum;
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
      pragma Import (C, glTexImage1D, "glTexImage1D");
   begin  -- TexImage
      glTexImage1D (Target, Level, Internal_Format, Width, Border, Format, Pixel_Type, Pixels);
   end TexImage;

   procedure TexImage (Target          : in Enum;
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
      pragma Import (C, glTexImage2D, "glTexImage2D");
   begin  -- TexImage
      glTexImage2D (Target, Level, Internal_Format, Width, Height, Border, Format, Pixel_Type, Pixels);
   end TexImage;

   procedure TexImage (Target          : in Enum;
                       Level           : in Int;
                       Internal_Format : in Enum;
                       Width           : in SizeI;
                       Height          : in SizeI;
                       Depth           : in SizeI;
                       Border          : in Int;
                       Format          : in Enum;
                       Pixel_Type      : in Enum;
                       Pixels          : in System.Address) is
      procedure glTexImage3D (Target          : in Enum;
                              Level           : in Int;
                              Internal_Format : in Enum;
                              Width           : in SizeI;
                              Height          : in SizeI;
                              Depth           : in SizeI;
                              Border          : in Int;
                              Format          : in Enum;
                              Pixel_Type      : in Enum;
                              Pixels          : in System.Address);
      pragma Import (C, glTexImage3D, "glTexImage3D");
   begin  -- TexImage
      glTexImage3D (Target, Level, Internal_Format, Width, Height, Depth, Border, Format, Pixel_Type, Pixels);
   end TexImage;

   -- Texture coordinates
   procedure TexCoord (S : in Short) is
      procedure glTexCoord1s (S : in Short);
      pragma Import (C, glTexCoord1s, "glTexCoord1s");
   begin  -- TexCoord
      glTexCoord1s (S);
   end TexCoord;

   procedure TexCoord (S : in Int) is
      procedure glTexCoord1i (S : in Int);
      pragma Import (C, glTexCoord1i, "glTexCoord1i");
   begin  -- TexCoord
      glTexCoord1i (S);
   end TexCoord;

   procedure TexCoord (S : in Float) is
      procedure glTexCoord1f (S : in Float);
      pragma Import (C, glTexCoord1f, "glTexCoord1f");
   begin  -- TexCoord
      glTexCoord1f (S);
   end TexCoord;

   procedure TexCoord (S : in Double) is
      procedure glTexCoord1d (S : in Double);
      pragma Import (C, glTexCoord1d, "glTexCoord1d");
   begin  -- TexCoord
      glTexCoord1d (S);
   end TexCoord;

   procedure TexCoord (S : in Short;
                       T : in Short) is
      procedure glTexCoord2s (S : in Short;
                              T : in Short);
      pragma Import (C, glTexCoord2s, "glTexCoord2s");
   begin  -- TexCoord
      glTexCoord2s (S, T);
   end TexCoord;

   procedure TexCoord (S : in Int;
                       T : in Int) is
      procedure glTexCoord2i (S : in Int;
                              T : in Int);
      pragma Import (C, glTexCoord2i, "glTexCoord2i");
   begin  -- TexCoord
      glTexCoord2i (S, T);
   end TexCoord;

   procedure TexCoord (S : in Float;
                       T : in Float) is
      procedure glTexCoord2f (S : in Float;
                              T : in Float);
      pragma Import (C, glTexCoord2f, "glTexCoord2f");
   begin  -- TexCoord
      glTexCoord2f (S, T);
   end TexCoord;

   procedure TexCoord (S : in Double;
                       T : in Double) is
      procedure glTexCoord2d (S : in Double;
                              T : in Double);
      pragma Import (C, glTexCoord2d, "glTexCoord2d");
   begin  -- TexCoord
      glTexCoord2d (S, T);
   end TexCoord;

   procedure TexCoord (S : in Short;
                       T : in Short;
                       R : in Short) is
      procedure glTexCoord3s (S : in Short;
                              T : in Short;
                              R : in Short);
      pragma Import (C, glTexCoord3s, "glTexCoord3s");
   begin  -- TexCoord
      glTexCoord3s (S, T, R);
   end TexCoord;

   procedure TexCoord (S : in Int;
                       T : in Int;
                       R : in Int) is
      procedure glTexCoord3i (S : in Int;
                              T : in Int;
                              R : in Int);
      pragma Import (C, glTexCoord3i, "glTexCoord3i");
   begin  -- TexCoord
      glTexCoord3i (S, T, R);
   end TexCoord;

   procedure TexCoord (S : in Float;
                       T : in Float;
                       R : in Float) is
      procedure glTexCoord3f (S : in Float;
                              T : in Float;
                              R : in Float);
      pragma Import (C, glTexCoord3f, "glTexCoord3f");
   begin  -- TexCoord
      glTexCoord3f (S, T, R);
   end TexCoord;

   procedure TexCoord (S : in Double;
                       T : in Double;
                       R : in Double) is
      procedure glTexCoord3d (S : in Double;
                              T : in Double;
                              R : in Double);
      pragma Import (C, glTexCoord3d, "glTexCoord3d");
   begin  -- TexCoord
      glTexCoord3d (S, T, R);
   end TexCoord;

   procedure TexCoord (S : in Short;
                       T : in Short;
                       R : in Short;
                       Q : in Short) is
      procedure glTexCoord4s (S : in Short;
                              T : in Short;
                              R : in Short;
                              Q : in Short);
      pragma Import (C, glTexCoord4s, "glTexCoord4s");
   begin  -- TexCoord
      glTexCoord4s (S, T, R, Q);
   end TexCoord;

   procedure TexCoord (S : in Int;
                       T : in Int;
                       R : in Int;
                       Q : in Int) is
      procedure glTexCoord4i (S : in Int;
                              T : in Int;
                              R : in Int;
                              Q : in Int);
      pragma Import (C, glTexCoord4i, "glTexCoord4i");
   begin  -- TexCoord
      glTexCoord4i (S, T, R, Q);
   end TexCoord;

   procedure TexCoord (S : in Float;
                       T : in Float;
                       R : in Float;
                       Q : in Float) is
      procedure glTexCoord4f (S : in Float;
                              T : in Float;
                              R : in Float;
                              Q : in Float);
      pragma Import (C, glTexCoord4f, "glTexCoord4f");
   begin  -- TexCoord
      glTexCoord4f (S, T, R, Q);
   end TexCoord;

   procedure TexCoord (S : in Double;
                       T : in Double;
                       R : in Double;
                       Q : in Double) is
      procedure glTexCoord4d (S : in Double;
                              T : in Double;
                              R : in Double;
                              Q : in Double);
      pragma Import (C, glTexCoord4d, "glTexCoord4d");
   begin  -- TexCoord
      glTexCoord4d (S, T, R, Q);
   end TexCoord;

   procedure TexCoord (V : in Shorts_1) is
      procedure glTexCoord1sv (S : in Shorts_1);
      pragma Import (C, glTexCoord1sv, "glTexCoord1sv");
   begin  -- TexCoord
      glTexCoord1sv (V);
   end TexCoord;

   procedure TexCoord (V : in Shorts_2) is
      procedure glTexCoord2sv (S : in Shorts_2);
      pragma Import (C, glTexCoord2sv, "glTexCoord2sv");
   begin  -- TexCoord
      glTexCoord2sv (V);
   end TexCoord;

   procedure TexCoord (V : in Shorts_3) is
      procedure glTexCoord3sv (S : in Shorts_3);
      pragma Import (C, glTexCoord3sv, "glTexCoord3sv");
   begin  -- TexCoord
      glTexCoord3sv (V);
   end TexCoord;

   procedure TexCoord (V : in Shorts_4) is
      procedure glTexCoord4sv (S : in Shorts_4);
      pragma Import (C, glTexCoord4sv, "glTexCoord4sv");
   begin  -- TexCoord
      glTexCoord4sv (V);
   end TexCoord;

   procedure TexCoord (V : in Ints_1) is
      procedure glTexCoord1iv (S : in Ints_1);
      pragma Import (C, glTexCoord1iv, "glTexCoord1iv");
   begin  -- TexCoord
      glTexCoord1iv (V);
   end TexCoord;

   procedure TexCoord (V : in Ints_2) is
      procedure glTexCoord2iv (S : in Ints_2);
      pragma Import (C, glTexCoord2iv, "glTexCoord2iv");
   begin  -- TexCoord
      glTexCoord2iv (V);
   end TexCoord;

   procedure TexCoord (V : in Ints_3) is
      procedure glTexCoord3iv (S : in Ints_3);
      pragma Import (C, glTexCoord3iv, "glTexCoord3iv");
   begin  -- TexCoord
      glTexCoord3iv (V);
   end TexCoord;

   procedure TexCoord (V : in Ints_4) is
      procedure glTexCoord4iv (S : in Ints_4);
      pragma Import (C, glTexCoord4iv, "glTexCoord4iv");
   begin  -- TexCoord
      glTexCoord4iv (V);
   end TexCoord;

   procedure TexCoord (V : in Floats_1) is
      procedure glTexCoord1fv (S : in Floats_1);
      pragma Import (C, glTexCoord1fv, "glTexCoord1fv");
   begin  -- TexCoord
      glTexCoord1fv (V);
   end TexCoord;

   procedure TexCoord (V : in Floats_2) is
      procedure glTexCoord2fv (S : in Floats_2);
      pragma Import (C, glTexCoord2fv, "glTexCoord2fv");
   begin  -- TexCoord
      glTexCoord2fv (V);
   end TexCoord;

   procedure TexCoord (V : in Floats_3) is
      procedure glTexCoord3fv (S : in Floats_3);
      pragma Import (C, glTexCoord3fv, "glTexCoord3fv");
   begin  -- TexCoord
      glTexCoord3fv (V);
   end TexCoord;

   procedure TexCoord (V : in Floats_4) is
      procedure glTexCoord4fv (S : in Floats_4);
      pragma Import (C, glTexCoord4fv, "glTexCoord4fv");
   begin  -- TexCoord
      glTexCoord4fv (V);
   end TexCoord;

   procedure TexCoord (V : in Doubles_1) is
      procedure glTexCoord1dv (S : in Doubles_1);
      pragma Import (C, glTexCoord1dv, "glTexCoord1dv");
   begin  -- TexCoord
      glTexCoord1dv (V);
   end TexCoord;

   procedure TexCoord (V : in Doubles_2) is
      procedure glTexCoord2dv (S : in Doubles_2);
      pragma Import (C, glTexCoord2dv, "glTexCoord2dv");
   begin  -- TexCoord
      glTexCoord2dv (V);
   end TexCoord;

   procedure TexCoord (V : in Doubles_3) is
      procedure glTexCoord3dv (S : in Doubles_3);
      pragma Import (C, glTexCoord3dv, "glTexCoord3dv");
   begin  -- TexCoord
      glTexCoord3dv (V);
   end TexCoord;

   procedure TexCoord (V : in Doubles_4) is
      procedure glTexCoord4dv (S : in Doubles_4);
      pragma Import (C, glTexCoord4dv, "glTexCoord4dv");
   begin  -- TexCoord
      glTexCoord4dv (V);
   end TexCoord;

   ---------------------------------------------------------------------------

   procedure Vertex (X : in Short;
                     Y : in Short) is
      procedure glVertex2s (X : in Short;
                            Y : in Short);
      pragma Import (C, glVertex2s, "glVertex2s");
   begin  -- Vertex
      glVertex2s (X, Y);
   end Vertex;

   procedure Vertex (X : in Int;
                     Y : in Int) is
      procedure glVertex2i (X : in Int;
                            Y : in Int);
      pragma Import (C, glVertex2i, "glVertex2i");
   begin  -- Vertex
      glVertex2i (X, Y);
   end Vertex;

   procedure Vertex (X : in Float;
                     Y : in Float) is
      procedure glVertex2f (X : in Float;
                            Y : in Float);
      pragma Import (C, glVertex2f, "glVertex2f");
   begin  -- Vertex
      glVertex2f (X, Y);
   end Vertex;

   procedure Vertex (X : in Double;
                     Y : in Double) is
      procedure glVertex2d (X : in Double;
                            Y : in Double);
      pragma Import (C, glVertex2d, "glVertex2d");
   begin  -- Vertex
      glVertex2d (X, Y);
   end Vertex;

   procedure Vertex (X : in Short;
                     Y : in Short;
                     Z : in Short) is
      procedure glVertex3s (X : in Short;
                            Y : in Short;
                            Z : in Short);
      pragma Import (C, glVertex3s, "glVertex3s");
   begin  -- Vertex
      glVertex3s (X, Y, Z);
   end Vertex;

   procedure Vertex (X : in Int;
                     Y : in Int;
                     Z : in Int) is
      procedure glVertex3i (X : in Int;
                            Y : in Int;
                            Z : in Int);
      pragma Import (C, glVertex3i, "glVertex3i");
   begin  -- Vertex
      glVertex3i (X, Y, Z);
   end Vertex;

   procedure Vertex (X : in Float;
                     Y : in Float;
                     Z : in Float) is
      procedure glVertex3f (X : in Float;
                            Y : in Float;
                            Z : in Float);
      pragma Import (C, glVertex3f, "glVertex3f");
   begin  -- Vertex
      glVertex3f (X, Y, Z);
   end Vertex;

   procedure Vertex (X : in Double;
                     Y : in Double;
                     Z : in Double) is
      procedure glVertex3d (X : in Double;
                            Y : in Double;
                            Z : in Double);
      pragma Import (C, glVertex3d, "glVertex3d");
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
      pragma Import (C, glVertex4s, "glVertex4s");
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
      pragma Import (C, glVertex4i, "glVertex4i");
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
      pragma Import (C, glVertex4f, "glVertex4f");
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
      pragma Import (C, glVertex4d, "glVertex4d");
   begin  -- Vertex
      glVertex4d (X, Y, Z, W);
   end Vertex;

   procedure Vertex (V : in Shorts_2) is
      procedure glVertex2sv (V : in Shorts_2);
      pragma Import (C, glVertex2sv, "glVertex2sv");
   begin  -- Vertex
      glVertex2sv (V);
   end Vertex;

   procedure Vertex (V : in Shorts_3) is
      procedure glVertex3sv (V : in Shorts_3);
      pragma Import (C, glVertex3sv, "glVertex3sv");
   begin  -- Vertex
      glVertex3sv (V);
   end Vertex;

   procedure Vertex (V : in Shorts_4) is
      procedure glVertex4sv (V : in Shorts_4);
      pragma Import (C, glVertex4sv, "glVertex4sv");
   begin  -- Vertex
      glVertex4sv (V);
   end Vertex;

   procedure Vertex (V : in Ints_2) is
      procedure glVertex2iv (V : in Ints_2);
      pragma Import (C, glVertex2iv, "glVertex2iv");
   begin  -- Vertex
      glVertex2iv (V);
   end Vertex;

   procedure Vertex (V : in Ints_3) is
      procedure glVertex3iv (V : in Ints_3);
      pragma Import (C, glVertex3iv, "glVertex3iv");
   begin  -- Vertex
      glVertex3iv (V);
   end Vertex;

   procedure Vertex (V : in Ints_4) is
      procedure glVertex4iv (V : in Ints_4);
      pragma Import (C, glVertex4iv, "glVertex4iv");
   begin  -- Vertex
      glVertex4iv (V);
   end Vertex;

   procedure Vertex (V : in Floats_2) is
      procedure glVertex2fv (V : in Floats_2);
      pragma Import (C, glVertex2fv, "glVertex2fv");
   begin  -- Vertex
      glVertex2fv (V);
   end Vertex;

   procedure Vertex (V : in Floats_3) is
      procedure glVertex3fv (V : in Floats_3);
      pragma Import (C, glVertex3fv, "glVertex3fv");
   begin  -- Vertex
      glVertex3fv (V);
   end Vertex;

   procedure Vertex (V : in Floats_4) is
      procedure glVertex4fv (V : in Floats_4);
      pragma Import (C, glVertex4fv, "glVertex4fv");
   begin  -- Vertex
      glVertex4fv (V);
   end Vertex;

   procedure Vertex (V : in Doubles_2) is
      procedure glVertex2dv (V : in Doubles_2);
      pragma Import (C, glVertex2dv, "glVertex2dv");
   begin  -- Vertex
      glVertex2dv (V);
   end Vertex;

   procedure Vertex (V : in Doubles_3) is
      procedure glVertex3dv (V : in Doubles_3);
      pragma Import (C, glVertex3dv, "glVertex3dv");
   begin  -- Vertex
      glVertex3dv (V);
   end Vertex;

   procedure Vertex (V : in Doubles_4) is
      procedure glVertex4dv (V : in Doubles_4);
      pragma Import (C, glVertex4dv, "glVertex4dv");
   begin  -- Vertex
      glVertex4dv (V);
   end Vertex;

   ---------------------------------------------------------------------------

end Lumen.GL;
