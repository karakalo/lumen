with
     GL.Pointers,
     GL.lean,

     ada.characters.latin_1,
     ada.Strings.unbounded,
     ada.Text_IO,

     interfaces.c.Strings;


package body opengl.Program
is

   use gl.lean,
       Interfaces;

   compiling_in_debug_Mode : constant Boolean := True;



   type Shader_view is access all opengl.Shader.item'class;

   --  Utility
   --

   function textFileRead (FileName : in String)       return c.Char_array;
   function to_String    (Self     : in c.char_array) return String;



   --  Parameters
   --

   procedure Program_is (Self : in out Parameters;   Now : in opengl.Program.view)
   is
   begin
      self.Program := Now;
   end Program_is;


   function Program (Self : in Parameters) return opengl.Program.view
   is
   begin
      return self.Program;
   end Program;




   --  Program
   --

   --  define/destroy
   --

   procedure define  (Self : in out Item;   use_vertex_Shader   : in opengl.Shader.view;
                                            use_fragment_Shader : in opengl.Shader.view)
   is
      use C;
   begin
      self.gl_Program := glCreateProgram;

      glAttachShader (self.gl_Program,  use_vertex_Shader  .gl_Shader);
      glAttachShader (self.gl_Program,  use_fragment_Shader.gl_Shader);

      self.vertex_Shader   := use_vertex_Shader;
      self.fragment_Shader := use_fragment_Shader;

--        declare
--           attribute_Name : c.char_array := "bone_Ids" & c.Char'First;
--        begin
--           glBindAttribLocation (self.gl_Program,  1,  attribute_Name (attribute_Name'First)'unchecked_access);
--        end;
--
--        declare
--           attribute_Name : c.char_array := "bone_Weights" & c.Char'First;
--        begin
--           glBindAttribLocation (self.gl_Program,  3,  attribute_Name (attribute_Name'First)'unchecked_access);
--        end;

--        glBindAttribLocation (self.gl_Program, 0, new_String ("vertex_Id" & ada.characters.Latin_1.NUL));
--        glBindAttribLocation (self.gl_Program, 0, new_String ("v_Id"));


      glLinkProgram (Self.gl_Program);
      declare
         Status : aliased gl.glInt;
      begin
         glGetProgramiv (self.gl_Program,  GL_LINK_STATUS,  Status'Unchecked_Access);

         if Status = 0 then
            declare
               link_Log : constant String := self.ProgramInfoLog;
            begin
               Self.destroy;
               raise opengl.Error with "program link " & link_Log;
            end;
         end if;
      end;

      if compiling_in_debug_Mode then   glValidateProgram (self.gl_Program);   end if;
   end define;



   procedure define  (Self : in out Item;   use_vertex_Shader_Filename   : in String;
                                            use_fragment_Shader_Filename : in String)
   is
      use opengl.Shader;
      the_vertex_Shader   : constant Shader_view := new opengl.Shader.item;
      the_fragment_Shader : constant Shader_view := new opengl.Shader.item;
   begin
      the_vertex_Shader  .define (opengl.shader.Vertex,   use_vertex_Shader_Filename);
      the_fragment_Shader.define (opengl.shader.Fragment, use_fragment_Shader_Filename);

      self.define (the_vertex_Shader  .all'Access,
                   the_fragment_Shader.all'Access);
   end define;



   procedure destroy (Self : in out Item)
   is
   begin
      glDeleteProgram (self.gl_Program);
   end destroy;




   --  Attributes
   --

   function Attribute (Self : access Item'Class;   Named : in String) return opengl.Attribute.view
   is
   begin
      for Each in 1 .. Self.attribute_Count
      loop
         if Self.Attributes (Each).Name = Named then
            return Self.Attributes (Each);
         end if;
      end loop;

      raise Error with "'" & Named & "' is not a valid program attribute.";
   end Attribute;



   function attribute_Location (Self : access Item'Class;   Named : in String) return gl.GLuint
   is
      use GL.Pointers;
      use type gl.GLint;
      attribute_Name :          c.strings.chars_ptr := Interfaces.C.Strings.new_String (Named & ada.characters.Latin_1.NUL);
      gl_Location    : constant gl.GLint            := glGetAttribLocation (self.gl_Program,
                                                                            to_GLchar_access (attribute_Name));
   begin
      if gl_Location = -1 then
         raise Error with "Requested attribute '" & Named & "' has no gl location in program.";
      end if;

      Interfaces.C.Strings.free (attribute_Name);
      return gl.GLuint (gl_Location);
   end attribute_Location;



   function is_defined (Self : in Item'Class) return Boolean
   is
      use type a_gl_Program;
   begin
      return self.gl_Program /= 0;
   end is_defined;



   function ProgramInfoLog (Self : in Item) return String
   is
      use C, GL;
      infologLength : aliased glInt   := 0;
      charsWritten  : aliased glSizei := 0;
   begin
      glGetProgramiv (self.gl_Program,  GL_INFO_LOG_LENGTH, infologLength'Unchecked_Access);

      if infologLength = 0 then
         return "";
      end if;

      declare
         use GL.Pointers;
         infoLog     : aliased  C.char_array        := C.char_array' (1 .. C.size_t (infoLogLength) => <>);
         infoLog_Ptr : constant C.strings.chars_ptr := C.strings.to_chars_ptr (infoLog'Unchecked_Access);
      begin
         glGetProgramInfoLog (self.gl_Program,
                              glSizei (infologLength),
                              charsWritten'Unchecked_Access,
                              to_GLchar_access (infoLog_Ptr));
         return to_String (infoLog);
      end;
   end ProgramInfoLog;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return opengl.Variable.uniform.int
   is
      use opengl.Variable.uniform;
      the_Variable : opengl.Variable.uniform.int;
   begin
      define (the_Variable, Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return opengl.Variable.uniform.float
   is
      use opengl.Variable.uniform;
      the_Variable : opengl.Variable.uniform.float;
   begin
      define (the_Variable, Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return opengl.Variable.uniform.vec3
   is
      use opengl.Variable.uniform;
      the_Variable : opengl.Variable.uniform.vec3;
   begin
      define (the_Variable, Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return opengl.Variable.uniform.vec4
   is
      use opengl.Variable.uniform;
      the_Variable : opengl.Variable.uniform.vec4;
   begin
      define (the_Variable, Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return opengl.Variable.uniform.mat3
   is
      use opengl.Variable.uniform;
      the_Variable : opengl.Variable.uniform.mat3;
   begin
      define (the_Variable, Self, Named);
      return the_Variable;
   end uniform_Variable;



   function uniform_Variable (Self : access Item'Class;   Named : in String) return opengl.Variable.uniform.mat4
   is
      use opengl.Variable.uniform;
      the_Variable : opengl.Variable.uniform.mat4;
   begin
      define (the_Variable, Self, Named);
      return the_Variable;
   end uniform_Variable;




   --  Operations
   --

   procedure add (Self : in out Item;   the_Attribute : in opengl.Attribute.view)
   is
   begin
      Self.attribute_Count                   := Self.attribute_Count + 1;
      Self.Attributes (Self.attribute_Count) := the_Attribute;
   end add;



   procedure enable (Self : in Item)
   is
   begin
      glUseProgram (self.gl_Program);
   end enable;



   procedure enable_Attributes (Self : in Item)
   is
      use type GL.GLuint;
   begin
      for Each in 1 .. Self.attribute_Count
      loop
         Self.Attributes (Each).enable;
      end loop;
   end enable_Attributes;



   procedure mvp_Matrix_is (Self : in out Item'Class;   Now : in Matrix_4x4)
   is
   begin
      Self.mvp_Matrix := Now;
   end mvp_Matrix_is;



   procedure inverse_modelview_Matrix_is (Self : in out Item'Class;   Now : in Matrix_3x3)
   is
   begin
      Self.inverse_modelview_Matrix := Now;
   end inverse_modelview_Matrix_is;



   procedure directional_Light_is (Self : in out Item'Class;   Now : in openGL.Light.directional.item)
   is
   begin
      Self.directional_Light := Now;
   end directional_Light_is;



   procedure Scale_is (Self : in out Item'Class;   Now : in Vector_3)
   is
   begin
      Self.Scale := Now;
   end Scale_is;



   procedure set_mvp_Uniform (Self : in Item)
   is
      the_mvp_Uniform : constant opengl.Variable.uniform.mat4 := Self.uniform_Variable ("mvp_Matrix");
   begin
      the_mvp_Uniform.Value_is (Self.mvp_Matrix);
   end set_mvp_Uniform;



   --  Privvy
   --

   function gl_Program (Self : in Item) return a_gl_Program
   is
   begin
      return self.gl_Program;
   end gl_Program;



   --  Utility
   --

   NL : constant String := ada.characters.latin_1.CR & ada.characters.latin_1.LF;


   function textFileRead (FileName : in String) return c.Char_array
   is
      use ada.Strings.unbounded, ada.text_io;

      the_File  : ada.text_io.File_type;
      Pad       : unbounded_String;
   begin
      open (the_File, in_File, Filename);

      while not end_of_File (the_File)
      loop
         append (Pad, get_Line (the_File) & NL);
      end loop;

      close (the_File);

      declare
         use type interfaces.c.size_t;
         the_Data   : C.char_array (1 .. C.size_t (Length (Pad)) + 1);
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


end opengl.Program;
