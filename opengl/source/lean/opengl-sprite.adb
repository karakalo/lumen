package body opengl.Sprite
is

   procedure define (Self : in out Item;   Faces     : access Geometry.views;
                                           Transform : in     opengl.Matrix_4x4 := Identity_4x4)
   is
   begin
      self.Faces     := Faces;
      self.Transform := Transform;
   end define;

end opengl.Sprite;
