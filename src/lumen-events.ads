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

   function To_UTF_8 (Symbol : in Key_Symbol) return String;
   function To_Symbol (Char : in Character) return Key_Symbol;

end Lumen.Events;
