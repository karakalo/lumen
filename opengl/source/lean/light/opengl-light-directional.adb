with
     openGL.Math;


package body openGL.Light.directional
is

   procedure Site_is (Self : in out Item;   Now                    : in Vector_3;
                                            inverse_view_Transform : in Matrix_3x3)
   is
      use openGL.Math;
      light_Site : Vector_3 renames Now;
   begin
      Self.Direction        := Normalised (light_Site) * inverse_view_Transform;
      Self.halfplane_Vector := Normalised (Normalised (Self.Direction (1 .. 3)) + (0.0, 0.0, 1.0));
      --  tbd: do this properly (for specular highlights in openGL vertex shaders).
--        Self.halfplane_Vector := Normalised (Normalised (light_Site) + (0.0, 0.0, 1.0));
   end Site_is;



   procedure Color_is (Self : in out Item;   Ambient  : in Vector_4;
                                             Diffuse  : in Vector_4;
                                             Specular : in Vector_4)
   is
   begin
      Self.ambient_Color  := Ambient;
      Self.diffuse_Color  := Diffuse;
      Self.specular_Color := Specular;
   end Color_is;


end openGL.Light.directional;


