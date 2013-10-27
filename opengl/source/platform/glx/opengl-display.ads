with
     Interfaces.C;


package opengl.Display
--
--  Models an openGL display.
--
is

   type Item is tagged private;


   function Default                    return Item;
   function screen_Id (Self : in Item) return interfaces.C.int;



private

   use Interfaces;


   type Item is tagged
      record
         --           Connection : xcb_connection_t_Pointer;
         --           Xcb        : access Standard.xcb.Display;
         screen_Id  : aliased interfaces.C.int;
         --           eGL           : standard.eGL.EGLDisplay;

         --           Version_major,
         --           Version_minor : aliased standard.eGL.EGLint;
      end record;

end opengl.Display;


