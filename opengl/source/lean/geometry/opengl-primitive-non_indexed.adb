with
     openGL.Errors,
     GL.lean;


package body opengl.Primitive.non_indexed
is


   --  Forge
   --

   overriding
   procedure define (Self : in out Item;   Kind : in facet_Kind)
   is
   begin
      Self.facet_Kind := Kind;
   end define;



   function new_Primitive (Kind         : in facet_Kind;
                           vertex_Count : in Natural) return Primitive.non_indexed.view
   is
      Self : constant View := new Item;
   begin
      define (Self.all,  Kind);
      Self.vertex_Count := vertex_Count;

      return Self;
   end new_Primitive;



   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end destroy;



   --------------
   --  Attributes
   --

   overriding
   procedure Indices_are  (Self : in out Item;   Now : in Indices)
   is
   begin
      raise Program_Error with "Trying to set indices in a *non-indexed* openGL primitive.";
   end Indices_are;



   --------------
   --  Operations
   --

   overriding
   procedure render (Self : in out Item)
   is
      use GL, GL.lean;
   begin
      glDrawArrays (Thin (Self.facet_Kind),
                    0,
                    gl.GLint (Self.vertex_Count));

--        glDrawElements (Thin     (self.facet_Kind),
--                        gl.GLint (self.Indices.Length),
--                        GL_UNSIGNED_SHORT,
--                        system.null_Address);
      openGL.Errors.log;
   end render;


end opengl.Primitive.non_indexed;
