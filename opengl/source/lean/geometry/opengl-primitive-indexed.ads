private
with
     openGL.Buffer.indices;

package opengl.Primitive.indexed
--
--  Provides a class for indexed opengl primitives.
--
is

   type    Item  is limited new Primitive.item with private;
   subtype Class is Item'Class;

   type    View  is access all Item'class;
   type    Views is array (Index_t range <>) of View;


   ---------
   --  Forge
   --

   function  new_Primitive (Kind    : in facet_Kind;
                            Indices : in openGL.Indices) return Primitive.indexed.view;

   procedure define  (Self : in out Item;   Kind    : in facet_Kind;
                                            Indices : in openGL.Indices);
   overriding
   procedure destroy (Self : in out Item);



   --------------
   --  Attributes
   --

   overriding
   procedure Indices_are  (Self : in out Item;   Now : in Indices);



   --------------
   --  Operations
   --

   overriding
   procedure render (Self : in out Item);



private

   type Item  is limited new Primitive.item with
      record
         Indices : Buffer.indices.view;
      end record;

end opengl.Primitive.indexed;
