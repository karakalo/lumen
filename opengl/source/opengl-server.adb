with
     GL.lean,
     interfaces.C.strings,
     ada.unchecked_Conversion;

package body openGL.Server
is


   function Version return String
   is
      use GL, gl.lean,  Interfaces;

      type GLubyte_Pointer is access all GLubyte;
      function to_chars_ptr is new ada.Unchecked_Conversion (GLubyte_Pointer,
                                                             c.strings.chars_ptr);
      the_Version : constant String := c.strings.Value (to_chars_ptr (glGetString (GL_VERSION)));
   begin
      return the_Version;
   end Version;



   function Version return a_Version
   is
      the_Version : constant String := Version;
   begin
      return (major => Integer'Value (the_Version (1 .. 1)),
              minor => Integer'Value (the_Version (3 .. 3)));
   end Version;


end openGL.Server;
