with
     opengl.surface_Profile,
     opengl.Display;

private
with
     Glx;

limited
with
     openGL.Context;


package opengl.Surface
--
--  modela sn openGL surface.
--
is

   type Item  is tagged private;
   type Items is array (Positive range <>) of aliased Item;

   type View  is access all Item'class;
   type Views is array (Positive range <>) of View;


   procedure define (Self : in out Item;   surface_Profile : in opengl.surface_Profile.item'Class;
                                           Display         : in opengl.Display.Item;
                                           Window_Id       : in Natural);


   --  Operations
   --
   procedure swap_Buffers (Self : in Item);



private

   type Item is tagged
      record
         glx_Surface :        glx.GLXDrawable;
         Context     : access openGL.Context.item'Class;
         Display     :        openGL.Display.item;
      end record;

end opengl.Surface;
