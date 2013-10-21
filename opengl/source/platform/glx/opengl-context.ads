with
     openGL.Display,
     openGL.surface_Profile,
     openGL.Surface,

     Glx.GLXContext;


package openGL.Context
--
--  Models an openGL (GLX) context.
--
is
   --  pragma Pure;


   type Item is tagged private;
   type View is access all Item'Class;


   procedure define (Self : in out Item;   the_Display         : access opengl.Display        .item'Class;
                                           the_surface_Profile : in     opengl.surface_Profile.item'Class);
--                                             the_Surface         : in     openGL.Surface.view);


   procedure make_Current (Self : in Item;   read_Surface  : in opengl.Surface.item;
                                             write_Surface : in opengl.Surface.item);


   function glx_Context_debug (Self : in Item'Class) return GLX.GLXContext.Item;   -- xcb.xcb_glx_context_t;
   --  tbd: move this to privvy pkg.



private

   type Item is tagged
      record
         glx_Context : aliased  GLX.GLXContext.item;            -- xcb.xcb_glx_context_t;
         Display     : access   opengl.Display.item'Class;
      end record;

end openGL.Context;
