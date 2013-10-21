with
     GL.lean,
     System,
     Ada.Unchecked_Conversion;


package body opengl.Attribute
is

   use GL.lean;


   --  Forge
   --

   procedure define  (Self : in out Item)
   is
   begin
      null;
   end define;


   procedure destroy (Self : in out Item)
   is
   begin
      null;
   end destroy;



   package body Forge
   is
      function to_Attribute (Name        : in String;
                             gl_Location : in gl.GLuint;
                             Size        : in gl.GLint;
                             data_Kind   : in Attribute.data_Kind;
                             Stride      : in Natural;
                             Offset      : in system.Storage_Elements.Storage_Offset;
                             Normalized  : in Boolean) return Item
      is
      begin
         return (name          => new String'(Name),
                 location      => gl_Location,
                 size          => Size,
                 data_kind     => data_Kind,
                 vertex_stride => gl.GLint (Stride),
                 offset        => Offset,
                 normalized    => Boolean'Pos (Normalized));
      end to_Attribute;


      function new_Attribute (Name        : in String;
                              gl_Location : in gl.GLuint;
                              Size        : in gl.GLint;
                              data_Kind   : in Attribute.data_Kind;
                              Stride      : in Natural;
                              Offset      : in system.Storage_Elements.Storage_Offset;
                              Normalized  : in Boolean) return View
      is
      begin
         return new Item'(to_Attribute (Name, gl_Location, Size, data_Kind, Stride, Offset, Normalized));
      end new_Attribute;

   end Forge;





   --  Sttributes
   --

   function Name (Self : in Item'Class) return String
   is
   begin
      return Self.Name.all;
   end Name;



   function gl_Location (Self : in Item'Class) return gl.GLuint
   is
   begin
      return self.Location;
   end gl_Location;



   --  Operations
   --

   procedure enable (Self : in Item)
   is
      use GL;
      type GLvoid_access is access all GLvoid;

      function to_GL is new ada.unchecked_Conversion (attribute.data_Kind,                      gl.GLenum);
      function to_GL is new ada.unchecked_Conversion (system.Storage_Elements.Storage_Offset,   GLvoid_access);
   begin
      glEnableVertexAttribArray (index      => Self.gl_Location);
      glVertexAttribPointer     (indx       => Self.gl_Location,
                                 size       => Self.Size,
                                 the_type   => to_GL (Self.data_Kind),
                                 normalized => Self.Normalized,
                                 stride     => Self.vertex_Stride,
                                 ptr        => to_GL (Self.Offset));
   end enable;


end opengl.Attribute;
