package Lumen.Events is
   -- Translated keysym type and value
   type Key_Category is (Key_Control, Key_Graphic, Key_Modifier, Key_Function, Key_Special, Key_Unknown, Key_Not_Translated);
   subtype Key_Symbol is Long_Integer;

   -- Keystroke and pointer modifiers
   type Modifier is (Mod_Shift, Mod_Lock, Mod_Control, Mod_1, Mod_2, Mod_3, Mod_4, Mod_5,
                     Mod_Button_1, Mod_Button_2, Mod_Button_3, Mod_Button_4, Mod_Button_5);
   type Modifier_Set is array (Modifier) of Boolean;
   No_Modifiers : Modifier_Set := (others => False);

   Not_Character : exception;  -- key symbol is not a Latin-1 character

   ---------------------------------------------------------------------------

   -- Key translation helpers

   -- Convert a Key_Symbol into a Latin-1 character; raises Not_Character if
   -- it's not possible.  Character'Val is simpler.
   function To_Character (Symbol : in Key_Symbol) return Character;

   -- Convert a Key_Symbol into a UTF-8 encoded string; raises Not_Character
   -- if it's not possible.  Really only useful for Latin-1 hibit chars, but
   -- works for all Latin-1 chars.
   function To_UTF_8 (Symbol : in Key_Symbol) return String;

   -- Convert a normal Latin-1 character to a Key_Symbol
   function To_Symbol (Char : in Character) return Key_Symbol;

end Lumen.Events;
