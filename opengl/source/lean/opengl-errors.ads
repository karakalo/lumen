package openGL.Errors
--
--  Provides utilities for displaying openGL errors.
--
is

   function Current return String;
   --
   --  Returns a descriptive string of the last occuring openGL error.
   --  Returns "", when no error exists.
   --  Clears any existing error.

   procedure log (Prefix : in String := "");
   --
   --  Displays 'Current' error via ada.Text_IO.put_Line.
   --  Clears any existing error.
   --  Raises 'openGL_Error' when an opengl error has been detected.

   procedure log (Prefix : in String := ""; error_Occurred : out Boolean);
   --
   --  Displays 'Current' via ada.Text_IO.put_Line.
   --  Clears any existing error.
   --  Sets error_Occurred to true, if a GL error was detected.

end opengl.Errors;
