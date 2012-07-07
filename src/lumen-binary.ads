
-- Useful types for handling binary data on 32- and 64-bit platforms
--
-- Chip Richards, Phoenix AZ, April 2007


-- For purposes of this package:
--
-- byte  --  8 bits (octet, C char, GNAT Character/Short_Short_Integer)
-- short -- 16 bits (C short, GNAT Short_Integer)
-- word  -- 32 bits (C int/long(32), GNAT Integer/Long_Integer)
-- long  -- 64 bits (C long(64), GNAT Long_Long_Integer)
--
-- Note that these are not identical to the names/definitions used on pretty
-- much any extant 32- or 64-bit platform, though they are somewhat biased to
-- early 21st-century 32-bit platforms.


package Lumen.Binary is

   ---------------------------------------------------------------------------
   --
   -- Public constants
   --
   ---------------------------------------------------------------------------

   -- Basic structure (sizes) of fundamental binary data types
   Byte_Bits   : constant :=  8;
   Short_Bits  : constant := 16;
   Word_Bits   : constant := 32;
   Long_Bits   : constant := 64;

   -- Derived sizes
   Short_Bytes : constant := Short_Bits / Byte_Bits;
   Word_Bytes  : constant := Word_Bits  / Byte_Bits;
   Long_Bytes  : constant := Long_Bits  / Byte_Bits;

   -- "Last-bit" values for use in rep clauses
   Byte_LB     : constant := Byte_Bits  - 1;
   Short_LB    : constant := Short_Bits - 1;
   Word_LB     : constant := Word_Bits  - 1;
   Long_LB     : constant := Long_Bits  - 1;

   ---------------------------------------------------------------------------
   --
   -- Public types
   --
   ---------------------------------------------------------------------------

   -- Unsigned types
   type Byte  is mod 2 **  Byte_Bits;
   type Short is mod 2 **  Short_Bits;
   type Word  is mod 2 **  Word_Bits;
   type Long  is mod 2 **  Long_Bits;

   for Byte'Size    use  Byte_Bits;
   for Short'Size   use  Short_Bits;
   for Word'Size    use  Word_Bits;
   for Long'Size    use  Long_Bits;

   -- Signed types
   Byte_Exp  : constant := Byte_Bits  - 1;
   Short_Exp : constant := Short_Bits - 1;
   Word_Exp  : constant := Word_Bits  - 1;
   Long_Exp  : constant := Long_Bits  - 1;

   type S_Byte  is new Integer range -(2 ** Byte_Exp)  .. +((2 ** Byte_Exp)  - 1);
   type S_Short is new Integer range -(2 ** Short_Exp) .. +((2 ** Short_Exp) - 1);
   type S_Word  is new Integer range -(2 ** Word_Exp)  .. +((2 ** Word_Exp)  - 1);
   type S_Long  is new Long_Long_Integer;  -- must use this for now; fixme later

   for S_Byte'Size   use  Byte_Bits;
   for S_Short'Size  use  Short_Bits;
   for S_Word'Size   use  Word_Bits;
   for S_Long'Size   use  Long_Bits;

   -- Array types built from the above basic types
   type Byte_String   is array (Natural range <>) of Byte;
   type Short_String  is array (Natural range <>) of Short;
   type Word_String   is array (Natural range <>) of Word;
   type Long_String   is array (Natural range <>) of Long;

   type S_Byte_String  is array (Natural range <>) of S_Byte;
   type S_Short_String is array (Natural range <>) of S_Short;
   type S_Word_String  is array (Natural range <>) of S_Word;
   type S_Long_String  is array (Natural range <>) of S_Long;

   -- Useful byte-string types for data conversion
   subtype Short_Byte_String is Byte_String (1 .. Short_Bytes);
   subtype Word_Byte_String  is Byte_String (1 .. Word_Bytes);
   subtype Long_Byte_String  is Byte_String (1 .. Long_Bytes);

   ---------------------------------------------------------------------------

end Lumen.Binary;
