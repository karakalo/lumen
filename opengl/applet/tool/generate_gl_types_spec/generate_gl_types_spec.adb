with
     Ada.Strings.unbounded,
     Ada.Strings.fixed,
     Ada.Strings.Maps,
     Ada.Characters.Latin_1,
     Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors;


procedure generate_GL_types_Spec
--
--  Generates the 'gl_types.ads' package which contains openGL types whose definitions may differ amongst platforms.
--
--  For cross-compilation purposes, the 1st argument (if present) can specify the 'include' folder in which to look
--  for the GL headers. The default is '/usr/include'.
--
is
   use Ada.Strings.unbounded,
       Ada.Command_Line,
       Ada.Text_IO;

   package lines_Vectors is new Ada.Containers.Vectors (Positive,
                                                        unbounded_String);
   the_Lines      : lines_Vectors.Vector;
   headers_Prefix : String              := (if    argument_Count = 0 then "/usr/include"
                                            else                          Argument (1));
begin
   --  Find GL types in 'GL/gl.h'.
   --
   declare
      the_File : File_type;
   begin
      open (the_File,  in_File,  headers_Prefix & "/GL/gl.h");

      while not End_of_File (the_File)
      loop
         declare
            the_Line : unbounded_String := to_unbounded_String  (get_Line (the_File));
         begin
            if    Index (the_Line, "typedef") /= 0
              and Index (the_Line, "(")        = 0
            then
               if   Index (the_Line, "GLenum")     /= 0
                 or Index (the_Line, "GLboolean")  /= 0
                 or Index (the_Line, "GLbitfield") /= 0
                 or Index (the_Line, "GLvoid")     /= 0
                 or Index (the_Line, "GLbyte")     /= 0
                 or Index (the_Line, "GLshort")    /= 0
                 or Index (the_Line, "GLint")      /= 0
                 or Index (the_Line, "GLubyte")    /= 0
                 or Index (the_Line, "GLushort")   /= 0
                 or Index (the_Line, "GLuint")     /= 0
                 or Index (the_Line, "GLsizei")    /= 0
                 or Index (the_Line, "GLfloat")    /= 0
                 or Index (the_Line, "GLclampf")   /= 0
                 or Index (the_Line, "GLdouble")   /= 0
                 or Index (the_Line, "GLclampd")   /= 0
               then
                  the_Lines.append ((the_Line));
               end if;
            end if;
         end;
      end loop;

      close (the_File);
   end;


   --  Find GL types in 'GLES2/gl2.h'.
   --
   declare
      the_File : File_type;
   begin
      open (the_File,  in_File,  headers_Prefix & "/GLES2/gl2.h");

      while not End_of_File (the_File)
      loop
         declare
            the_Line : unbounded_String := to_unbounded_String  (get_Line (the_File));
         begin
            if    Index (the_Line, "typedef") /= 0
              and Index (the_Line, "(")        = 0
            then
               if   Index (the_Line, "GLchar")  /= 0
                 or Index (the_Line, "GLfixed") /= 0
               then
                  the_Lines.append ((the_Line));
               end if;
            end if;
         end;
      end loop;

      close (the_File);
   end;


   --  Generate the 'gl_types.ads' Ada package.
   --
   declare
      use lines_Vectors,
          ada.Strings.Maps,
          ada.Strings;

      the_File : File_type;
      Cursor   : lines_Vectors.Cursor := the_Lines.First;


      function to_Ada (the_Line : in unbounded_String) return String
      is
         Whitespace_Set : constant Character_Set := to_Set (" " & Ada.Characters.Latin_1.HT);
         semicolon_Pos  : constant Integer       := Index  (the_Line,  ";",  going => Backward);

         Last           :          Integer       := semicolon_Pos - 1;
         First          :          Integer;


         function GL_Type return String
         is
         begin
            First := Index (the_Line, Whitespace_Set,  going => Backward,
                                                       from  => Last);
            declare
               the_Slice : String  := Slice       (the_Line,  First + 1, Last);
               pad_Size  : Integer := Integer'Max (0,         10 - the_Slice'Length);
            begin
               return to_String (  the_Slice
                                 & pad_Size * ' ');  -- Padding.
            end;
         end GL_Type;

         the_GL_Type : String := GL_Type;


         function C_Type return String
         is
            use ada.Strings.fixed;
         begin
            Last  := Index (the_Line,  Whitespace_Set,  from  => First,
                                                        going => Backward,
                                                        test  => Outside);
            First := Index (the_Line,  Whitespace_Set,  from  => 9,
                                                        going => Backward,
                                                        test  => Outside);
            return Slice (the_Line,  First, Last);
         end C_Type;

         the_C_Type : String := C_Type;


         function the_Ada_Type return String
         is
         begin
            if    the_C_Type = "int"             then   return "C.int";
            elsif the_C_Type = "unsigned int"    then   return "C.unsigned";
            elsif the_C_Type = "signed char"     then   return "C.signed_char";
            elsif the_C_Type = "char"            then   return "C.char";
            elsif the_C_Type = "unsigned char"   then   return "C.unsigned_char";
            elsif the_C_Type = "short"           then   return "C.short";
            elsif the_C_Type = "unsigned short"  then   return "C.unsigned_short";
            elsif the_C_Type = "float"           then   return "C.C_float";
            elsif the_C_Type = "double"          then   return "C.double";
            elsif the_C_Type = "khronos_int32_t" then   return "Integer_32";
            elsif the_C_Type = "void"            then   return "system.Address";
            else
               raise Program_Error with "Unhandled C type: '" & the_c_Type & "'";
            end if;
         end the_Ada_Type;

      begin
         return "   subtype " & the_GL_Type & " is " & the_Ada_Type & ";";
      end to_Ada;


   begin
      create (the_File, out_File, "../../../private/gl/source/gl_types.ads");

      put_Line (the_File,  "with");
      put_Line (the_File,  "     interfaces.C,");
      put_Line (the_File,  "     System;");
      new_Line (the_File);
      put_Line (the_File,  "package GL_Types");
      put_Line (the_File,  "--");
      put_Line (the_File,  "--  Provides openGL types whose definitions may differ amongst platforms.");
      put_Line (the_File,  "--");
      put_Line (the_File,  "--  This file is generated by the 'generate_GL_types_Spec' tool.");
      put_Line (the_File,  "--");
      put_Line (the_File,  "is");
      put_Line (the_File,  "   pragma Pure;");
      put_Line (the_File,  "   use Interfaces;");
      new_Line (the_File);

      while has_Element (Cursor)
      loop
         put_Line (the_File,  to_Ada (Element (Cursor)));
         next (Cursor);
      end loop;

      new_Line (the_File);
      put_Line (the_File,  "end GL_Types;");

      close (the_File);
   end;

end generate_GL_types_Spec;
