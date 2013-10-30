with
     opengl.Program,
     GL.lean,
     GL.Pointers,
     Interfaces.C.Strings;


package body opengl.Variable.uniform
is

   use GL.lean,
       Interfaces;


   --  Forge
   --

   procedure define  (Self : in out Item;   Program : access opengl.Program.item'Class;
                                            Name    : in     String)
   is
      use GL.Pointers, C;
      the_Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.new_String (Name);
   begin
      self.gl_Variable := glGetUniformLocation (Program.gl_Program,
                                                to_GLchar_access (the_Name));
      Interfaces.C.Strings.free (the_Name);

      if self.gl_Variable = -1
      then
         raise openGL.Error with "unable to get location for uniform named '" & Name & "'";
      end if;
   end define;



   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end destroy;




   -----------
   --  Actuals
   --


   --  int
   --
   procedure Value_is (Self : in int;   Now : in Integer)
   is
   begin
      glUniform1i (self.gl_Variable,  gl.GLint (Now));
   end Value_is;



   --  float
   --
   procedure Value_is (Self : in float;   Now : in opengl.Real)
   is
      use GL, opengl;
      the_Float : aliased opengl.Real := Now;
   begin
      glUniform1fv (self.gl_Variable,
                    1,
                    the_Float'Unchecked_Access);
   end Value_is;



   --  vec3
   --
   procedure Value_is (Self : in vec3;   Now : in opengl.Vector_3)
   is
      use GL;
      the_Vector : aliased opengl.Vector_3 := Now;
   begin
      glUniform3fv (self.gl_Variable,
                    1,
                    the_Vector (1)'Unchecked_Access);
   end Value_is;



   --  vec4
   --
   procedure Value_is (Self : in vec4;   Now : in opengl.Vector_4)
   is
      use GL;
      the_Vector : aliased opengl.Vector_4 := Now;
   begin
      glUniform4fv (self.gl_Variable,
                    1,
                    the_Vector (1)'Unchecked_Access);
   end Value_is;



   --  mat3
   --
   procedure Value_is (Self : in mat3;   Now : in opengl.Matrix_3x3)
   is
      use GL;
      the_Matrix : aliased opengl.Matrix_3x3 := Now;
   begin
      glUniformMatrix3fv (self.gl_Variable,
                          1,
                          GL_FALSE,
                          the_Matrix (1, 1)'Unchecked_Access);
   end Value_is;



   --  mat4
   --
   procedure Value_is (Self : in mat4;   Now : in opengl.Matrix_4x4)
   is
      use GL;
      the_Matrix : aliased opengl.Matrix_4x4 := Now;
   begin
      glUniformMatrix4fv (self.gl_Variable,
                          1,
                          GL_FALSE,
                          the_Matrix (1, 1)'Unchecked_Access);
   end Value_is;


end opengl.Variable.uniform;
