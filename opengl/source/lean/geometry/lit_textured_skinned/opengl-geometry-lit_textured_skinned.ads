with
     openGL.Program.lit_textured_skinned;

package opengl.Geometry.lit_textured_skinned   -- tbd: Rename to 'lit_colored_textured_skinned'.
--
--  Supports per-vertex site, color, texture, lighting and skinning.
--
is

   type Item is new openGL.Geometry.item with private;


   --  Vertex
   --
   type Vertex is
      record
         Site   : Vector_3;
         Normal : Vector_3;
         Color  : lucid_Color;
         Coords : Coordinate_2D;

         bone_Ids     : Vector_4;
         bone_Weights : Vector_4;
      end record;
   pragma Convention (C, Vertex);

   type Vertex_array is array (Index_t range <>) of aliased Vertex;



   --  Forge
   --

   function new_Geometry return access Geometry.lit_textured_skinned.item'class;



   --  Attributes
   --

   overriding
   function  is_Transparent (Self : in     Item) return Boolean;



   --  Operations
   --

   procedure Vertices_are (Self : in out Item'Class;   Now : access Vertex_array);

   overriding
   procedure Indices_are  (Self : in out Item;         Now       : in Indices;
                                                       for_Facia : in Positive);

   function Program return openGL.Program.lit_textured_skinned.view;



private

   type Item is new Geometry.item with
      record
         null;
      end record;

   overriding
   procedure enable_Texture (Self : in Item);

end opengl.Geometry.lit_textured_skinned;
