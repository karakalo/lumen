with
     openGL.Display,
     openGL.surface_Profile,
     openGL.Surface,
     EGL;

package openGL.Context
--
--  Models an openGL context.
--
is
   --  pragma Pure;

   type Item is tagged private;
   type View is access all Item'Class;


   procedure define       (Self : in out Item;   the_Display         : access opengl.Display.item'Class;
                                                 the_surface_Profile : in     opengl.surface_Profile.item);

   procedure make_Current (Self : in     Item;   read_Surface        : in     opengl.Surface.item;
                                                 write_Surface       : in     opengl.Surface.item);

   function  egl_Context_debug
                          (Self : in     Item'Class) return egl.EGLConfig;   -- tbd: move this to privvy pkg.



private

   type Item is tagged
      record
         egl_Context : aliased egl.EGLContext;
         Display     : access  opengl.Display.item'Class;
      end record;

end openGL.Context;
