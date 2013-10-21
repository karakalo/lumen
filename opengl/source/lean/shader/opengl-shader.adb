with
     GL.lean,
     GL.Pointers,

     ada.characters.latin_1,
     ada.Strings.unbounded,
     ada.Text_IO,

     interfaces.c.Strings;


package body opengl.Shader
is
   use GL.lean,
       Interfaces;


   --  Utility
   --
   function textFileRead (FileName : in String)       return c.Char_array;
   function to_String    (Self     : in c.char_array) return String;



   --  Forge
   --

   procedure define (Self : in out Item;   Kind            : in shader.Kind;
                                           source_Filename : in String)
   is
      use C,
          GL.Pointers;
      the_Source     : aliased          C.char_array        := textFileRead           (source_Filename);
      the_Source_ptr : aliased constant C.strings.chars_ptr := C.strings.to_chars_ptr (the_Source'Unchecked_Access);
      the_Source_Array : aliased C.Strings.chars_ptr_array  := (1 => the_Source_ptr);
   begin
      Self.Kind := Kind;

      if Kind = Vertex then
         Self.gl_Shader := glCreateShader (GL_VERTEX_SHADER);
      else
         Self.gl_Shader := glCreateShader (GL_FRAGMENT_SHADER);
      end if;

--        glShaderSource  (Self.gl_Shader, 1, to_GLchar_access (the_Source_ptr), null);
      glShaderSource  (Self.gl_Shader, 1, to_GLchar_Pointer_access (the_Source_array'Access), null);
      glCompileShader (Self.gl_Shader);

      declare
         Status : aliased gl.glInt;
      begin
         glGetShaderiv (self.gl_Shader,  GL_COMPILE_STATUS,  Status'Unchecked_Access);

         if Status = 0 then
            declare
               compile_Log : constant String := self.ShaderInfoLog;
            begin
               Self.destroy;
               raise opengl.Error with "'" & source_Filename & "' compilation " & compile_Log;
            end;
         end if;
      end;
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
      glDeleteShader (self.gl_Shader);
   end destroy;




   --  Attributes
   --

   function ShaderInfoLog (Self : in Item) return String
   is
      use C, GL;
      infologLength : aliased glInt   := 0;
      charsWritten  : aliased glSizei := 0;
   begin
      glGetShaderiv (self.gl_Shader,  GL_INFO_LOG_LENGTH,  infologLength'Unchecked_Access);

      if infologLength = 0 then
         return "";
      end if;

      declare
         use gl.Pointers;
         infoLog     : aliased  C.char_array        := C.char_array' (1 .. C.size_t (infoLogLength) => <>);
         infoLog_Ptr : constant c.strings.chars_Ptr := C.strings.to_chars_ptr (infoLog'Unchecked_Access);
      begin
         glGetShaderInfoLog (self.gl_Shader,
                             glSizei (infologLength),
                             charsWritten'Unchecked_Access,
                             to_GLchar_access (infoLog_Ptr));

         return to_String (infoLog);
      end;
   end ShaderInfoLog;



   --  Privvy
   --

   function gl_Shader (Self : in Item) return a_gl_Shader
   is
   begin
      return self.gl_Shader;
   end gl_Shader;





   --  Utility
   --
   NL : constant String := "" & ada.characters.latin_1.LF;


   function textFileRead (FileName : in String) return c.Char_array
   is
      use ada.text_IO,
          ada.Strings.unbounded;

      the_File  : ada.text_io.File_type;
      Pad       : unbounded_String;
   begin
      open (the_File, in_File, Filename);

      while not end_of_File (the_File)
      loop
         append (Pad,  get_Line (the_File) & NL);
      end loop;

      close (the_File);

      declare
         use type interfaces.c.size_t;
         the_Data : C.char_array (1 .. C.size_t (Length (Pad)) + 1);
      begin
         for Each in 1 .. the_Data'Last - 1
         loop
            the_Data (Each) := C.char (Element (Pad, Integer (Each)));
         end loop;

         the_Data (the_Data'Last) := c.Char'Val (0);
         return the_Data;
      end;
   end textFileRead;



   function to_String (Self : in c.char_array) return String
   is
      use C;
      the_String : String (1 .. Self'Length);
   begin
      for Each in the_String'Range
      loop
         the_String (Each) := Character (Self (c.size_t (Each)));
      end loop;

      return the_String;
   end to_String;


end opengl.Shader;
