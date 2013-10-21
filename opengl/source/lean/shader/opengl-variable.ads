with
     GL;

package opengl.Variable
--
--  Models an openGL shader variable.
--
is
--     pragma Remote_Types;

   type Item  is abstract tagged private;


   --  Forge
   --
   procedure define  (Self : in out Item);
   procedure destroy (Self : in out Item);



private

   type Item is abstract tagged
      record
         gl_Variable : gl.GLint;
      end record;

end opengl.Variable;
