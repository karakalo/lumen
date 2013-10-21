with
     Freetype_C.Binding,
     Freetype_C.Pointers;


package body freetype.face_Size
is
   use Freetype_C;


   --- Attributes
   --

   function CharSize (Self : access Item;   face         : in FT_Face.item;
                                            point_size   : in Natural;
                                            x_resolution,
                                            y_resolution : in Natural)
                      return Boolean
   is
      use Freetype_C.Binding;
      use type FT_Error,
               FT_F26Dot6;
   begin
      if        Self.size        /= point_size
        or else Self.xResolution /= x_resolution
        or else Self.yResolution /= y_resolution
      then
         Self.err := FT_Set_Char_Size (face,
                                       0,                           FT_F26Dot6 (point_size) * 64,
                                       FT_UInt (Self.xResolution),  FT_UInt (Self.yResolution));
         if Self.err = 0 then
            Self.ftFace      := face;
            Self.size        := point_size;
            Self.xResolution := x_resolution;
            Self.yResolution := y_resolution;
            Self.ftSize      := FT_Face_Get_Size (Self.ftFace).all'Access;
         end if;
      end if;

      return Self.err = 0;
   end CharSize;



   function CharSize (Self : in Item) return Natural
   is
   begin
      return Self.size;
   end CharSize;



   function Ascender (Self : in Item) return Float
   is
      use Freetype_C.Binding,
          Freetype_C.Pointers;
   begin
      if Self.ftSize = null then
         return 0.0;
      else
         return Float (FT_Size_Get_Metrics (Self.ftSize).ascender) / 64.0;
      end if;
   end Ascender;



   function Descender (Self : in Item) return Float
   is
      use Freetype_C.Binding,
          Freetype_C.Pointers;
   begin
      if Self.ftSize = null then
         return 0.0;
      else
         return Float (FT_Size_Get_Metrics (Self.ftSize).descender) / 64.0;
      end if;
   end Descender;



   function Height (Self : in Item) return Float
   is
      use Freetype_C.Binding,
          Freetype_C.Pointers;
      use type FT_Long;
   begin
      if Self.ftSize = null then
         return 0.0;
      end if;

      if FT_Face_IS_SCALABLE (Self.ftFace) /= 0
      then
         return   Float (FT_Face_Get_BBox (Self.ftFace).yMax  -  FT_Face_Get_BBox (Self.ftFace).yMin)
                * (Float (FT_Size_Get_Metrics (Self.ftSize).y_ppem)  /  Float (FT_Face_Get_units_per_EM (Self.ftFace)));
      else
         return Float (FT_Size_Get_Metrics (Self.ftSize).height) / 64.0;
      end if;
   end Height;



   function Width (Self : in Item) return Float
   is
      use Freetype_C.Binding,
          Freetype_C.Pointers;
      use type FT_Long,
               FT_SizeRec_Pointer;
   begin
      if Self.ftSize = null then
         return 0.0;
      end if;

      if FT_Face_IS_SCALABLE (Self.ftFace) /= 0
      then
         return   Float (FT_Face_Get_BBox (Self.ftFace).xMax  -  FT_Face_Get_BBox (Self.ftFace).xMin)
                * (Float (FT_Size_Get_Metrics (Self.ftSize).x_ppem)  /  Float (FT_Face_Get_units_per_EM (Self.ftFace)));
      else
         return Float (FT_Size_Get_Metrics (Self.ftSize).max_advance) / 64.0;
      end if;
   end Width;



   function Underline (Self : in Item) return Float
   is
      pragma Unreferenced (Self);
   begin
      return 0.0;
   end Underline;



   function Error (Self : in Item) return FT_Error
   is
   begin
      return Self.err;
   end Error;


end freetype.face_Size;
