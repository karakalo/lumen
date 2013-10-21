limited
with
     opengl.Program;


package opengl.Variable.uniform
--
--  Models a uniform variable for openGL shaders.
--
is
--     pragma Remote_Types;

   type Item  is new opengl.Variable.item with private;



   --  Forge
   --

   procedure define  (Self : in out Item;   Program : access opengl.Program.item'class;
                                            Name    : in     String);
   overriding
   procedure destroy (Self : in out Item);



   --  Actuals
   --

   type int   is new Variable.uniform.item with private;
   type float is new Variable.uniform.item with private;
   type vec3  is new Variable.uniform.item with private;
   type vec4  is new Variable.uniform.item with private;
   type mat3  is new Variable.uniform.item with private;
   type mat4  is new Variable.uniform.item with private;

   procedure Value_is (Self : in int;     Now : in Integer);
   procedure Value_is (Self : in float;   Now : in opengl.Real);
   procedure Value_is (Self : in vec3;    Now : in opengl.Vector_3);
   procedure Value_is (Self : in vec4;    Now : in opengl.Vector_4);
   procedure Value_is (Self : in mat3;    Now : in opengl.Matrix_3x3);
   procedure Value_is (Self : in mat4;    Now : in opengl.Matrix_4x4);



private

   type Item  is new opengl.Variable.item with
      record
         null;
      end record;


   type int   is new Variable.uniform.item with null record;
   type float is new Variable.uniform.item with null record;

   type vec3  is new Variable.uniform.item with null record;
   type vec4  is new Variable.uniform.item with null record;

   type mat3  is new Variable.uniform.item with null record;
   type mat4  is new Variable.uniform.item with null record;

end opengl.Variable.uniform;
