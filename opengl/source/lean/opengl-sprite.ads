with
     opengl.Geometry;

package opengl.Sprite
--
-- Provides a sprite class.
--
is

   type Item is tagged private;


   procedure define (Self : in out Item;   Faces     : access Geometry.views;
                                           Transform : in     opengl.Matrix_4x4 := Identity_4x4);



private

   type Item is tagged
      record
         Faces     : access Geometry.views;
         Transform :        opengl.Matrix_4x4;
      end record;

end opengl.Sprite;
