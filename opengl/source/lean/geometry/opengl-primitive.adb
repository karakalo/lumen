with
     ada.Unchecked_Deallocation;

package body opengl.Primitive
is

   --  Forge
   --

   procedure define (Self : in out Item;   Kind    : in facet_Kind)
   is
   begin
      Self.facet_Kind := Kind;
   end define;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.Unchecked_Deallocation (Primitive.item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;



   --  Attributes
   --

   function Texture (Self : in Item) return opengl.Texture.Object
   is
   begin
      return self.Texture;
   end Texture;



   procedure Texture_is (Self : in out Item;   Now : in opengl.Texture.Object)
   is
   begin
      self.Texture := Now;
   end Texture_is;



   function Bounds (self : in Item) return opengl.Bounds
   is
   begin
      return self.Bounds;
   end Bounds;



   procedure Bounds_are (Self : in out Item;   Now : in opengl.Bounds)
   is
   begin
      self.Bounds := Now;
   end Bounds_are;



   function is_Transparent (self : in Item) return Boolean
   is
   begin
      return self.is_Transparent;
   end is_Transparent;



   procedure is_Transparent (Self : in out Item;   Now : in Boolean := True)
   is
   begin
      self.is_Transparent := Now;
   end is_Transparent;



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
--                                                                    (to_Buffer (buffer_Indices'access,
--                                                                                usage => opengl.buffer.static_Draw));
--     end;


end opengl.Primitive;
