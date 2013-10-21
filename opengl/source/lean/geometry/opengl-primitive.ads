with
     openGL.Texture;

private
with
     ada.Unchecked_Conversion;


package opengl.Primitive
--
--  Provides a base class for opengl primitives.
--
is

   type    Item  is abstract tagged limited private;
   subtype Class is Item'Class;

   type    View  is access all Item'class;
   type    Views is array (Index_t range <>) of View;



   --  Facets
   --
   type facet_Kind is (Points,
                       Lines,     line_Loop,      line_Strip,
                       Triangles, triangle_Strip, triangle_Fan);



   ---------
   --  Forge
   --

   procedure define  (Self : in out Item;   Kind    : in facet_Kind);
   procedure destroy (Self : in out Item)                             is abstract;
   procedure free    (Self : in out View);



   --------------
   --  Attributes
   --

   function  Texture        (Self : in     Item)     return opengl.Texture.Object;
   procedure Texture_is     (Self : in out Item;   Now : in opengl.Texture.Object);

   procedure Bounds_are     (Self : in out Item;   Now : in opengl.Bounds);
   function  Bounds         (self : in     Item)     return opengl.Bounds;
   --
   --  returns the bounds in Object space.

   procedure is_Transparent (Self : in out Item;   Now : in Boolean := True);
   function  is_Transparent (Self : in     Item)     return Boolean;



   ---------------
   --- Operations
   --

   procedure Indices_are  (Self : in out Item;   Now       : in Indices)   is abstract;
   procedure render       (Self : in out Item)                             is abstract;



private

   type Item is abstract tagged limited
      record
         facet_Kind     : primitive.facet_Kind;
         Texture        : opengl.Texture.Object := opengl.Texture.null_Object;
         is_Transparent : Boolean;
         Bounds         : opengl.Bounds;
      end record;


   --  Facets
   --

   function Thin (Self : in facet_Kind) return gl.GLenum;

   for facet_Kind use (Points         => gl.GL_POINTS,
                       Lines          => gl.GL_LINES,
                       line_Loop      => gl.GL_LINE_LOOP,
                       line_Strip     => gl.GL_LINE_STRIP,
                       Triangles      => gl.GL_TRIANGLES,
                       triangle_Strip => gl.GL_TRIANGLE_STRIP,
                       triangle_Fan   => gl.GL_TRIANGLE_FAN);

   for facet_Kind'Size use gl.GLenum'Size;

   function Convert_1 is new ada.Unchecked_Conversion (facet_Kind, gl.GLenum);

   function Thin (Self : in facet_Kind) return gl.GLenum
                  renames Convert_1;


end opengl.Primitive;
