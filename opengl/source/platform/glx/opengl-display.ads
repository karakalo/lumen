with
     Interfaces.C;

private
with
     swig.Pointers;


package opengl.Display
--
--  Models an openGL display.
--
is

   type Item is tagged private;


   function Default                    return Item;
   function screen_Id (Self : in Item) return interfaces.c.int;



private

   use Interfaces;


   type Item is tagged
      record
         --           Connection : xcb_connection_t_Pointer;
         --           Xcb        : access Standard.xcb.Display;
         screen_Id  : swig.Pointers.int_Pointer := new interfaces.c.int;
         --           eGL           : standard.eGL.EGLDisplay;

         --           Version_major,
         --           Version_minor : aliased standard.eGL.EGLint;
      end record;

end opengl.Display;


