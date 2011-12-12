
-- Lumen.GLU -- Lumen's own thin OpenGL utilities bindings
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


-- Environment
with System;

with Lumen.GL;
use  Lumen.GL;
with Lumen.Image;


package Lumen.GLU is

   -- Build mipmaps
   function Build_1D_Mipmaps (Target     : in Enum;
                            Components : in Int;
                            Width      : in Int;
                            Format     : in Enum;
                            Data_Type  : in Enum;
                            Data       : in System.Address)
   return Int;

   function Build_2D_Mipmaps (Target     : in Enum;
                            Components : in Int;
                            Width      : in Int;
                            Height     : in Int;
                            Format     : in Enum;
                            Data_Type  : in Enum;
                            Data       : in System.Address)
   return Int;

   -- Projections
   procedure Ortho_2D (Left   : in Double;
                      Right  : in Double;
                      Bottom : in Double;
                      Top    : in Double);

   procedure Perspective (FOV_Y_Angle, Aspect, Z_Near, Z_Far : in Double);

   -- Quadrics
   type Quadric is new Pointer;

   function New_Quadric return Quadric;

   procedure Delete_Quadric (Quad : in Quadric);

   procedure Quadric_Draw_Style (Quad : in Quadric;
                               Draw : in Enum);

   procedure Quadric_Normals (Quad   : in Quadric;
                             Normal : in Enum);

   procedure Quadric_Orientation (Quad        : in Quadric;
                                 Orientation : in Enum);

   procedure Quadric_Texture (Quad    : in Quadric;
                             Texture : in Bool);

   -- Shapes
   procedure Cylinder (Quad   : in Quadric;
                       Base   : in Double;
                       Top    : in Double;
                       Height : in Double;
                       Slices : in Int;
                       Stacks : in Int);

   procedure Disk (Quad   : in Quadric;
                   Inner  : in Double;
                   Outer  : in Double;
                   Slices : in Int;
                   Loops  : in Int);

   procedure Partial_Disk (Quad   : in Quadric;
                          Inner  : in Double;
                          Outer  : in Double;
                          Slices : in Int;
                          Loops  : in Int;
                          Start  : in Double;
                          Sweep  : in Double);

   procedure Sphere (Quad   : in Quadric;
                     Radius : in Double;
                     Slices : in Int;
                     Stacks : in Int);


private
   -- These can be bound directly
   pragma Import (C, Build_1D_Mipmaps, "gluBuild1DMipmaps");
   pragma Import (C, Build_2D_Mipmaps, "gluBuild2DMipmaps");
   pragma Import (C, Ortho_2D, "gluOrtho2D");
   pragma Import (C, Perspective, "gluPerspective");
   pragma Import (C, New_Quadric, "gluNewQuadric");
   pragma Import (C, Delete_Quadric, "gluDeleteQuadric");
   pragma Import (C, Quadric_Draw_Style, "gluQuadricDrawStyle");
   pragma Import (C, Quadric_Orientation, "gluQuadricOrientation");
   pragma Import (C, Quadric_Normals, "gluQuadricNormals");
   pragma Import (C, Quadric_Texture, "gluQuadricTexture");
   pragma Import (C, Cylinder, "gluCylinder");
   pragma Import (C, Sphere, "gluSphere");
   pragma Import (C, Disk, "gluDisk");
   pragma Import (C, Partial_Disk, "gluPartialDisk");

   pragma Linker_Options ("-lGLU");

end Lumen.GLU;