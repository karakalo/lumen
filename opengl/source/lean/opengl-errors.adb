with
     ada.Text_IO;

package body openGL.Errors
is
   use GL;


   function Current return String
   is
      the_Error : constant GL.GLenum := glGetError;
   begin
      case the_Error is
         when GL.GL_NO_ERROR =>
            return "no error";

         when others =>
            return "unknown openGL error detected";
      end case;
   end Current;



   procedure log (Prefix : in String := "")
   is
      current_Error : constant String := Current;

      function error_Msg return String is
      begin
         if Prefix = "" then
            return "openGL error: '" & current_Error & "'";
         else
            return Prefix & ": '" & current_Error & "'";
         end if;
      end error_Msg;

   begin
      if current_Error = "no error" then
         return;
      end if;

      raise openGL.Error with error_Msg;
   end log;



   procedure log (Prefix : in String := ""; error_Occurred : out Boolean)
   is
      use ada.Text_IO;
      current_Error : constant String := Current;
   begin
      if current_Error = "no error" then
         error_Occurred := False;
         return;
      end if;

      error_Occurred := True;

      if Prefix = "" then
         put_Line ("openGL error: '" & current_Error & "'");
      else
         put_Line (Prefix & ": '" & current_Error & "'");
      end if;
   end log;


end opengl.Errors;
