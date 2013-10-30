with
     openGL.Errors,
     openGL.Buffer,
     openGL.Texture,

     GL.lean,
     ada.Unchecked_Deallocation;


package body opengl.Primitive.indexed
is

   --- Forge
   --

   procedure define (Self : in out Item;   Kind    : in facet_Kind;
                                           Indices : in openGL.Indices)
   is
      use openGL.Buffer.indices;
      buffer_Indices : aliased opengl.Indices := (Indices'Range => <>);
   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Indices (Each) - 1;   -- Adjust indices to zero-based-indexing for GL.
      end loop;

      Self.facet_Kind := Kind;
      Self.Indices    := new opengl.Buffer.indices.object' (to_Buffer (buffer_Indices'Access,
                                                                       usage => opengl.buffer.static_Draw));
   end define;



   function new_Primitive (Kind    : in facet_Kind;
                           Indices : in openGL.Indices) return Primitive.indexed.view
   is
      use openGL.Buffer.indices;
      Self : constant View := new Item;
   begin
      define (Self.all,  Kind, Indices);
      return Self;
   end new_Primitive;




   overriding procedure destroy (Self : in out Item)
   is
      procedure free is new ada.unchecked_Deallocation (Buffer.indices.Object'Class, Buffer.indices.view);
   begin
      buffer.destroy (Self.Indices.all);
      free (Self.Indices);
   end destroy;



   --------------
   --  Attributes
   --

   function Texture (Self : in Item'Class) return opengl.Texture.Object
   is
   begin
      return self.Texture;
   end Texture;



   procedure Texture_is (Self : in out Item'Class;   Now : in opengl.Texture.Object)
   is
   begin
      self.Texture := Now;
   end Texture_is;



   --------------
   --  Operations
   --

   overriding
   procedure Indices_are  (Self : in out Item;   Now : in Indices)
   is
      use openGL.Buffer.indices;
      buffer_Indices : aliased Indices := (Now'Range => <>);
   begin
      for Each in buffer_Indices'Range
      loop
         buffer_Indices (Each) := Now (Each) - 1;   -- Adjust indices to zero-based-indexing for GL.
      end loop;

      Self.Indices.set (to => buffer_Indices);


--        free (self.Facias (self.facia_Count).Indices);
--
--        for Each in buffer_Indices'range loop
--           buffer_Indices (Each) := Now (Each) - 1;   -- adjust indices to zero-based-indexing for GL.
--        end loop;
--
--        self.Facias (self.facia_Count).Indices := new opengl.Buffer.indices.object'
--                                                        (to_Buffer (buffer_Indices'access,
--                                                                    usage => opengl.buffer.static_Draw));
   end Indices_are;



   overriding
   procedure render (Self : in out Item)
   is
      use GL,
          GL.lean;
   begin
      Self.Indices.enable;
      openGL.Errors.log;

      glDrawElements (Thin     (self.facet_Kind),
                      gl.GLint (self.Indices.Length),
                      GL_UNSIGNED_SHORT,
                      null);
   end render;



--     procedure add_Facia (Self : in out Item;   facet_Kind  : in geometry.facet_Kind;
--                                                the_Indices : in Indices)
--     is
--        use openGL.Buffer.indices;
--        use type Real, Index_t;
--
--        buffer_Indices : aliased Indices := (the_Indices'range => <>);
--     begin
--        self.primitive_Count := self.primitive_Count + 1;
--
--        for Each in buffer_Indices'range loop
--           buffer_Indices (Each) := the_Indices (Each) - 1;   -- adjust indices to zero-based-indexing for GL.
--        end loop;
--
--        self.Primitives (self.primitive_Count).facet_Kind := facet_Kind;
--        self.Primitives (self.primitive_Count).Indices    := new opengl.Buffer.indices.object'
--                                                                   (to_Buffer (buffer_Indices'access,
--                                                                               usage => opengl.buffer.static_Draw));
--     end;


end opengl.Primitive.indexed;
