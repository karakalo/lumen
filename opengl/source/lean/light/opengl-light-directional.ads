package openGL.Light.directional
--
--  Models a directional openGL light.
--
is

   type Item is new Light.item with
      record
         Direction        : Vector_3;   -- Normalized light direction in eye space.
         halfplane_Vector : Vector_3;   -- Normalized half-plane vector.

         ambient_Color    : Vector_4;
         diffuse_Color    : Vector_4;
         specular_Color   : Vector_4;
      end record;


   procedure Site_is (Self : in out Item;   Now                    : in Vector_3;
                                            inverse_view_Transform : in Matrix_3x3);


   procedure Color_is (Self : in out Item;   Ambient  : in Vector_4;
                                             Diffuse  : in Vector_4;
                                             Specular : in Vector_4);

end openGL.Light.directional;


