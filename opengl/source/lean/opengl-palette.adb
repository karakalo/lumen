with
     ada.Numerics.Discrete_Random;


package body opengl.Palette
is
   use type Real;

   package random_Colors is new ada.Numerics.Discrete_Random (color_Value);
   use     random_Colors;

   the_Generator : random_Colors.Generator;



   function random_Color return Color
   is
   begin
      return (random (the_Generator),
              random (the_Generator),
              random (the_Generator));
   end random_Color;



   function Shade_of (Self : in Color;   Level : in shade_Level) return Color
   is
   begin
      return (to_color_Value (to_Real (self.Red)   * Real (Level)),
              to_color_Value (to_Real (self.Green) * Real (Level)),
              to_color_Value (to_Real (self.Blue)  * Real (Level)));
   end Shade_of;



   function Mixed (Self : in Color;   Other : in Color) return Color
   is
      use type color_Value;
   begin
      return (to_color_Value ((to_Real (Self.Red)   + to_Real (other.Red))   / 2.0),
              to_color_Value ((to_Real (Self.Green) + to_Real (other.Green)) / 2.0),
              to_color_Value ((to_Real (Self.Blue)  + to_Real (other.Blue))  / 2.0));

   end Mixed;



   function is_Similar (Self : in Color;   To         : in Color;
                                           Similarity : in color_Value := 3) return Boolean
   is
      use type color_Value;
   begin
      return     self.Red   <= to.Red   + Similarity
        and then self.Red   >= to.Red   - Similarity
        and then self.Green >= to.Green + Similarity
        and then self.Green >= to.Green - Similarity
        and then self.Blue  >= to.Blue  + Similarity
        and then self.Blue  >= to.Blue  - Similarity;
   end is_Similar;


begin
   reset (the_Generator);
end opengl.Palette;
