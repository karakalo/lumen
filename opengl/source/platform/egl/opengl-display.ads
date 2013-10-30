private
with
     eGL;


package openGL.Display
--
--  Models an openGL display.
--
is

   type Item is tagged private;

   function Default return Item;



private

   type Item is tagged
      record
         Thin          :         eGL.EGLDisplay;
         Version_major,
         Version_minor : aliased eGL.EGLint;
      end record;

end openGL.Display;


