package freetype_c.FT_Bitmap 
is
   use Interfaces;
   
   
   type Item is 
      record
         rows         : aliased c.int;
         width        : aliased c.int;
         pitch        : aliased c.int;
         buffer       : access  c.unsigned_char;
         num_grays    : aliased c.short;
         pixel_mode   : aliased c.char;
         palette_mode : aliased c.char;
         palette      : aliased System.Address;
      end record;

   type Item_array is array (C.Size_t range <>) of aliased FT_Bitmap.Item;


   type Pointer       is access all FT_Bitmap.Item;
   type Pointer_array is array (C.Size_t range <>) of aliased FT_Bitmap.Pointer;

   type pointer_Pointer is access all freetype_c.FT_Bitmap.Pointer;

end freetype_c.FT_Bitmap;
