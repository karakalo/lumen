with
     GL;

package body openGL.Viewport
is
   use GL;



   function Extent return Extent_2d
   is
      Extent : array (1 .. 4) of aliased gl.glInt;
   begin
      glGetIntegerv (gl_VIEWPORT,
                     Extent (1)'Unchecked_Access);

      return (Integer (Extent (3)),
              Integer (Extent (4)));
   end Extent;



   procedure Extent_is (Now : in Extent_2d)
   is
   begin
      glViewport (0, 0,
                  GLint (now.Width),
                  GLint (now.Height));
   end Extent_is;


end openGL.Viewport;
