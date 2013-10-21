private
with
     GL.lean,
     Ada.Unchecked_Conversion;

package openGL.Buffer
--
--  Models a buffer object.
--
is

   subtype a_Name is GL.GLuint;                                    -- An openGL vertex buffer 'Name', which is a natural integer.
   type    a_Kind is (array_Buffer, element_array_Buffer);
   type     Usage is (stream_Draw,  static_Draw,  dynamic_Draw);


   --  Buffer Object
   --
   type Object is abstract tagged limited private;
   type View   is access all Object'Class;

   procedure destroy (Self : in out Object'Class);
   procedure free    (Self : in out View);


   --  Attributes
   --

   function Name (Self : in Object) return Buffer.a_Name;
   function Kind (Self : in Object) return Buffer.a_Kind   is abstract;
   function Length (Self : in Object) return Positive;


   --  Operations
   --

   procedure enable  (Self : in     Object'Class);



   --  Derived 'array' and 'element array' classes.
   --

   type         array_Object is new Object with private;
   type element_array_Object is new Object with private;

   --
   --  refer to child packages, for specific buffers:
   --
   --  - gl.Buffer.vertex
   --  - gl.Buffer.texture_coords
   --  - gl.Buffer.normals
   --  - gl.Buffer.indices
   --
   --  (tbd: pixel pack/unpack buffers)



   --  Errors
   --

   no_platform_Support : exception;
   --
   --  Raised by buffer 'Map' functions when OS platform does not support GL
   --  Buffer objects.



private

   use GL.lean;


   --  a_Kind
   --
   for a_Kind use
     (array_Buffer         => GL_ARRAY_BUFFER,
      element_array_Buffer => GL_ELEMENT_ARRAY_BUFFER);

   for a_Kind'Size use GL.GLenum'Size;

   function to_GL_Enum is new Ada.Unchecked_Conversion (a_Kind, GL.GLenum);


   --  Usage
   --
   for Usage use
     (stream_Draw  => GL_STREAM_DRAW,
      static_Draw  => GL_STATIC_DRAW,
      dynamic_Draw => GL_DYNAMIC_DRAW);
   for Usage'Size use GL.GLenum'Size;

   function to_GL_Enum is new Ada.Unchecked_Conversion (Usage, GL.GLenum);


   --  Object
   --
   type Object is abstract tagged limited
      record
         Name   : aliased Buffer.a_Name := 0;
         Length :         Positive;
      end record;

   overriding
   function Kind (Self : in         array_Object) return Buffer.a_Kind;

   overriding
   function Kind (Self : in element_array_Object) return Buffer.a_Kind;

   type         array_Object is new Object with null record;
   type element_array_Object is new Object with null record;



   --  Support
   --
   procedure verify_Name (Self : in out Object'Class);


end opengl.Buffer;
