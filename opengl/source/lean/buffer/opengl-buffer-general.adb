with
     openGL.Errors,

     GL.Pointers;

--       Interfaces.C.Extensions;

package body openGL.Buffer.general
is


   --     function to_gl_Pointer      is new ada.unchecked_Conversion (Element_Pointers.Pointer, glvoid_Pointer);
   --     function to_element_Pointer is new ada.unchecked_Conversion (glvoid_Pointer,           Element_Pointers.Pointer);

   --     type Void_access is access all Interfaces.C.Extensions.void;
   --     function to_element_Pointer is new ada.unchecked_Conversion (Void_access,               Element_Pointers.Pointer);



   --  'vertex buffer' Object
   --

   function to_Buffer (From  : access constant Element_Array;
                       Usage : in              Buffer.Usage) return  Object
   is
      use GL.Pointers;
   begin
      return new_Buffer : Object
      do
         new_Buffer.Usage := Usage;

         verify_Name (new_Buffer);
         new_Buffer.Length := From'Length;

         enable       (new_Buffer);
         glBufferData (to_GL_Enum (Kind (new_Buffer)),
                       From.all'Size / 8,
                      +From (From'First)'Address,
                       to_GL_Enum (Usage));
      end return;
   end to_Buffer;



   function to_Buffer (From  : in Element_Array;
                       Usage : in Buffer.Usage) return  Object
   is
      use GL.Pointers;
   begin
      return new_Buffer : Object
      do
         new_Buffer.Usage := Usage;

         verify_Name (new_Buffer);
         new_Buffer.Length := From'Length;

         enable       (new_Buffer);
         glBufferData (to_GL_Enum (Kind (new_Buffer)),
                       From'Size / 8,
                      +From (From'First)'Address,
                       to_GL_Enum (Usage));
      end return;
   end to_Buffer;



   procedure set (Self : in out Object;   Position : in Positive     := 1;
                                          To       : in Element_Array)
   is
      use GL.Pointers;
--        use type GLsizeiptr;
      new_Vertices        : aliased  Element_Array := To;
      Vertex_Size_in_bits : constant Natural       := To (To'First)'Size;
   begin
      --  tbd: make this '>=' and raise 'constraint_Error' instead of openGL_Error if bounds are violated.
      if Self.Length = To'Length
      then
         enable          (Self);
         glBufferSubData (to_GL_Enum (Kind (Self)),
                          offset =>  GLintptr ((Position - 1) * Vertex_Size_in_bits / 8),
                          size   =>  new_Vertices'Size / 8,
                          data   => +new_Vertices (new_Vertices'First)'Address);
      else
         Self.destroy;

         verify_Name (Self);
         Self.Length := To'Length;

         enable       (Self);
         glBufferData (to_GL_Enum (Kind (Self)),
                       To'Size / 8,
                      +To (To'First)'Address,
                       to_GL_Enum (Self.Usage));
      end if;

      openGL.Errors.log;
   end set;



   procedure set (Self : in out Object;   Position : in              Positive     := 1;
                                          To       : access constant Element_Array)
   is
   begin
      Self.set (Position, To.all);
   end set;



   --     procedure set (Self : in out Object;   Position : in Positive     := 1;    -- tbd: make this raise 'constraint_Error' instead
   --                                                                                        of openGL_Error, when bounds are violated.
   --                                            To       : access constant Element_Array)
   --     is
   --        use type glSizeIPtr;
   --        new_Vertices        :          Element_Array renames To.all;
   --        Vertex_Size_in_bits : constant Natural            := To (To'First)'Size;
   --     begin
   --        enable (Self);
   --        glBufferSubData (to_gl_Enum (Kind (Self)),  offset => glIntPtr ((Position - 1) * Vertex_Size_in_bits / 8),
   --                                                    size   => new_Vertices'size / 8,
   --                                                    data   => new_Vertices (new_Vertices'First)'Address);
   --        opengl.errors.log;
   --     end;


   --     function  get (Self   : in    Object) return Element_Array
   --     is
   --        the_Map      :          read_only_Map'Class renames Map (Self);
   --        the_Vertices : constant Element_Array            := get (the_Map,
   --  Index'First, self.Length);
   --     begin
   --        release (the_Map);
   --        return the_Vertices;
   --     end;





   ----------------------
   --  Buffer Memory Maps
   --

   --  memory Maps  -  appear not to be available in GLES
   --

   --     procedure release (Self : in    memory_Map)
   --     is
   --        Status : constant gl.glBoolean := gl.glUnmapBufferOES (self.Kind);
   --     begin
   --        if Status /= GL_True then
   --           raise Corrupt_Buffer;
   --        end if;
   --     end;
   --
   --
   --
   --
   --     function  get (Self : in memory_Map;   Position : in Index) return
   --  Element
   --     is
   --        use Element_Pointers, interfaces.C;
   --        Start : constant element_pointers.Pointer := self.Data +
   --  ptrDiff_t (Position - 1);
   --     begin
   --        return Value (Start, 1) (1);
   --     end;
   --
   --
   --
   --     function  get (Self : in memory_Map;   Position : in Index;
   --                                            Count    : in Positive
   --) return Element_Array
   --     is
   --        use Element_Pointers, interfaces.C;
   --        Start : constant element_pointers.Pointer := self.Data +
   --  ptrDiff_t (Position - 1);
   --     begin
   --        return Value (Start, ptrDiff_t (Count));
   --     end;
   --
   --
   --
   --
   --     procedure set (Self : in     memory_Map;   Position : in     Index;
   --                                                To       : access Element)
   --     is
   --        use ogl.Geometry, Element_Pointers, interfaces.C;
   --     begin
   --        copy_Array (element_Pointers.Pointer (To),  self.Data + ptrDiff_t
   --(Position - 1),  1);
   --     end;
   --
   --
   --
   --     procedure set (Self : in     memory_Map;   Position : in Index;
   --                                                To       : in Element)
   --     is
   --        the_Vertex : aliased Element := To;
   --     begin
   --        set (Self, Position, to => the_Vertex'unchecked_access);
   --     end;
   --
   --
   --
   --
   --     -- read-only
   --
   --     function  Map (Self : in Object) return read_only_Map'Class
   --     is
   --        use ogl.Geometry;
   --        the_Map : read_only_Map;
   --     begin
   --        enable (Self);
   --
   --        the_Map.Data := to_element_Pointer (Void_Access (mapBuffer
   --(vbo_Target (Self),  gl_READ_ONLY)));
   --        if the_Map.Data = null then
   --           raise ogl.buffer.no_platform_Support;
   --        end if;
   --
   --        the_Map.Last       := Index (Self.Length);
   --        the_Map.vbo_Target := vbo_Target (Self);
   --
   --        return the_Map;
   --     end;
   --
   --
   --
   --
   --     function  get (Self : in read_only_Map;   Position : in Index)
   --  return Element
   --     is
   --     begin
   --        return get (memory_Map (Self), Position);
   --     end;
   --
   --
   --     function  get (Self : in read_only_Map;   Position : in Index;
   --                                                      Count    : in
   --  Positive              ) return Element_Array
   --     is
   --     begin
   --        return get (memory_Map (Self), Position, Count);
   --     end;
   --
   --
   --
   --
   --
   --     -- write-only
   --
   --     function Map (Self : access Object) return write_only_Map'Class
   --     is
   --        use ogl.Geometry;
   --        the_Map : write_only_Map;
   --     begin
   --        enable (Self.all);
   --
   --        the_Map.Data := to_element_Pointer (Void_access (mapBuffer
   --(vbo_Target (Self.all), gl_WRITE_ONLY)));
   --        if the_Map.Data = null then
   --           raise ogl.buffer.no_platform_Support;
   --        end if;
   --
   --        the_Map.Last       := Index (Self.Length);
   --        the_Map.vbo_Target := vbo_Target (Self.all);
   --
   --        return the_Map;
   --     end;
   --
   --
   --
   --     procedure set (Self : in     write_only_Map;   Position : in
   --  Index;
   --                                                           To       :
   --  access Element)
   --     is
   --     begin
   --        set (memory_Map (Self), Position, To);
   --     end;
   --
   --
   --     procedure set (Self : in     write_only_Map;   Position : in Index;
   --                                                           To       : in
   --  Element)
   --     is
   --     begin
   --        set (memory_Map (Self), Position, To);
   --     end;
   --
   --
   --
   --
   --
   --     -- read-write
   --
   --     function Map (Self : access Object) return read_write_Map'Class
   --     is
   --        use ogl.Geometry;
   --        the_Map : read_write_Map;
   --     begin
   --        enable (Self.all);
   --
   --        the_Map.Data := to_element_Pointer (Void_access (mapBuffer
   --(vbo_Target (Self.all), gl_READ_WRITE)));
   --        if the_Map.Data = null then
   --           raise ogl.buffer.no_platform_Support;
   --        end if;
   --
   --        the_Map.Last       := Index (Self.Length);
   --        the_Map.vbo_Target := vbo_Target (Self.all);
   --
   --        return the_Map;
   --     end;
   --
   --
   --
   --     function  get (Self : in read_write_Map;   Position : in Index)
   --  return Element
   --     is
   --     begin
   --        return get (memory_Map (Self), Position);
   --     end;
   --
   --
   --     function  get (Self : in read_write_Map;   Position : in Index;
   --                                                       Count    : in
   --  Positive              ) return Element_Array
   --     is
   --     begin
   --        return get (memory_Map (Self), Position, Count);
   --     end;
   --
   --
   --
   --     procedure set (Self : in     read_write_Map;   Position : in
   --  Index;
   --                                                           To       :
   --  access Element)
   --     is
   --     begin
   --        set (memory_Map (Self), Position, To);
   --     end;
   --
   --
   --     procedure set (Self : in     read_write_Map;   Position : in Index;
   --                                                           To       : in
   --  Element)
   --     is
   --     begin
   --        set (memory_Map (Self), Position, To);
   --     end;

end opengl.Buffer.general;
