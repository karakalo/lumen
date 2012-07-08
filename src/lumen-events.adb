package body Lumen.Events is

   function To_UTF_8 (Symbol : in Key_Symbol) return String is

      Result : String (1 .. 2);  -- as big as we can encode

   begin  -- To_UTF_8
      if Symbol not in Key_Symbol (Character'Pos (Character'First)) .. Key_Symbol (Character'Pos (Character'Last)) then
         raise Not_Character;
      end if;

      if Symbol < 16#7F# then
         -- 7-bit characters just pass through unchanged
         Result (1) := Character'Val (Symbol);
         return Result (1 .. 1);
      else
         -- 8-bit characters are encoded in two bytes
         Result (1) := Character'Val (16#C0# + (Symbol  /  2 ** 6));
         Result (2) := Character'Val (16#80# + (Symbol rem 2 ** 6));
         return Result;
      end if;
   end To_UTF_8;

   ---------------------------------------------------------------------------

   -- Convert a normal Latin-1 character to a Key_Symbol
   function To_Symbol (Char : in Character) return Key_Symbol is
   begin  -- To_Symbol
      return Key_Symbol (Character'Pos (Char));
   end To_Symbol;

   ---------------------------------------------------------------------------

end Lumen.Events;
