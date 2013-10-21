generic
   type base_Object   is new openGL.Buffer.Object with private;

   type Index         is range <>;
   type Element       is private;
   type Element_Array is array (Index range <>) of Element;

package openGL.Buffer.general
--
--  A generic for producing various types of openGL vertex buffer objects.
--
is

   type Object is new base_Object with private;
   type View   is access all Object'Class;


   --  Forge
   --
   function to_Buffer (From  : access constant Element_Array;
                       Usage : in              Buffer.Usage) return Object;

   function to_Buffer (From  : in              Element_Array;
                       Usage : in              Buffer.Usage) return  Object;


   --  Operations
   --

   procedure set (Self : in out Object;   Position : in              Positive     := 1;
                                          To       : in              Element_Array);

   procedure set (Self : in out Object;   Position : in              Positive     := 1;   -- tbd: make this raise 'constraint_Error' instead of
                                          To       : access constant Element_Array);      --      openGL_Error, when bounds are violated.

   --     function  get (Self : in     Object) return Element_Array;



   ----------------------
   --  Buffer Memory Maps
   --

   --  buffer memory map   -  appear to be absent from GLES
   --

   --     type memory_Map is abstract tagged private;
   --
   --     procedure release (Self : in    memory_Map);
   --     --
   --     -- 'release' must be called to release the buffers data back to the GL server.
   --     --
   --     -- May raise Corrupt_Buffer if the Buffer has become corrupt since the data
   --     -- was initially mapped. This can occur for system-specific reasons
   --        that affect the availability of graphics memory,
   --     -- such as screen mode changes. In such situations, the data store
   --        contents are undefined, and an application
   --     -- reinitialize the data store.
   --     --
   --     Corrupt_Buffer : exception;
   --
   --
   --
   --     type read_only_Map  is new memory_Map with private;
   --
   --  --   function  Map (Self : access Object) return read_only_Map'Class;
   --     function  Map (Self : in Object) return read_only_Map'Class;
   --
   --     function  get (Self : in read_only_Map;   Position : in Index)
   --  return Element;
   --     function  get (Self : in read_only_Map;   Position : in Index;
   --                                               Count    : in Positive)
   --  return Element_Array;
   --
   --
   --
   --     type write_only_Map is new memory_Map with private;
   --
   --     function  Map (Self : access Object) return write_only_Map'Class;
   --
   --     procedure set (Self : in write_only_Map;   Position : in     Index;
   --                                                To       : access Element);
   --     procedure set (Self : in write_only_Map;   Position : in     Index;
   --                                                To       : in     Element);
   --
   --
   --
   --     type read_write_Map is new memory_Map with private;
   --
   --     function  Map (Self : access Object) return read_write_Map'Class;
   --
   --     function  get (Self : in read_write_Map;   Position : in Index)
   --  return Element;
   --     function  get (Self : in read_write_Map;   Position : in Index;
   --                                                Count    : in Positive)
   --  return Element_Array;
   --
   --     procedure set (Self : in read_write_Map;   Position : in     Index;
   --                                                To       : access Element);
   --     procedure set (Self : in read_write_Map;   Position : in     Index;
   --                                                To       : in     Element);



private

   type Object is new base_Object with
      record
         Usage : Buffer.Usage;
      end record;


   default_Terminator : Element;       -- No 'i.c.Pointers' subprogram is called which uses this, so
                                       --    a default 'Element' should suffice.


   ----------------------
   --  Buffer Memory Maps
   --

   --     package element_Pointers is new interfaces.C.Pointers (Index,
   --  Element, Element_Array, default_Terminator);

   --     type memory_Map is abstract tagged
   --        record
   --           vbo_Target : buffer.a_Kind;
   --
   --           Data : element_pointers.Pointer;
   --           Last : Index;
   --        end record;
   --
   --
   --     type read_only_Map  is new memory_Map with null record;
   --     type write_only_Map is new memory_Map with null record;
   --     type read_write_Map is new memory_Map with null record;

end opengl.Buffer.general;
