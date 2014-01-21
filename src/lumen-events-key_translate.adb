
-- Lumen.Events.Key_Translate -- Translate X11 keysym to Lumen symbol
--
-- Chip Richards, NiEstu, Phoenix AZ, Summer 2010

-- This code is covered by the ISC License:
--
-- Copyright © 2010, NiEstu
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- The software is provided "as is" and the author disclaims all warranties
-- with regard to this software including all implied warranties of
-- merchantability and fitness. In no event shall the author be liable for any
-- special, direct, indirect, or consequential damages or any damages
-- whatsoever resulting from loss of use, data or profits, whether in an
-- action of contract, negligence or other tortious action, arising out of or
-- in connection with the use or performance of this software.


-- Environment
with Ada.Characters.Latin_1;
with Lumen.Events.Keys;        use Lumen.Events.Keys;


package body Lumen.Events.Key_Translate is

   -- Useful range boundaries and other markers
   Lo_Char        : constant Key_Symbol := Key_Symbol (Character'Pos (Character'First));
   Hi_Char        : constant Key_Symbol := Key_Symbol (Character'Pos (Character'Last));
   Lo_Graphic     : constant Key_Symbol := Key_Symbol (Character'Pos (Ada.Characters.Latin_1.Space));
   Lo_Spec_1      : constant Key_Symbol := 16#FF08#;  -- X11 backspace
   Hi_Spec_1      : constant Key_Symbol := 16#FF1B#;  -- X11 escape
   Lo_Spec_2      : constant Key_Symbol := Home;
   Hi_Spec_2      : constant Key_Symbol := KP_Equal;
   Lo_Mod         : constant Key_Symbol := Shift_L;
   Hi_Mod         : constant Key_Symbol := Hyper_R;
   Lo_Func        : constant Key_Symbol := F1;
   Hi_Func        : constant Key_Symbol := F35;
   Unknown_Symbol : constant Key_Symbol := Key_Symbol'Last;
   Self_Symbol    : constant Key_Symbol := Key_Symbol'Last - 1;

   -- Translation array for specials that turn into Latin-1 control
   -- characters, or themselves
   type Translation_Target is record
      Val : Key_Symbol;
      Cat : Key_Category;
   end record;
   type Special_1_Translation is array (Lo_Spec_1 .. Hi_Spec_1) of Translation_Target;
   Specials_1 : constant Special_1_Translation :=
      (
       16#FF08#    => (Key_Symbol (Character'Pos (Ada.Characters.Latin_1.BS)),  Key_Control),
       16#FF09#    => (Key_Symbol (Character'Pos (Ada.Characters.Latin_1.HT)),  Key_Control),
       16#FF0A#    => (Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LF)),  Key_Control),
       Clear       => (Clear,                                                   Key_Special),
       16#FF0D#    => (Key_Symbol (Character'Pos (Ada.Characters.Latin_1.CR)),  Key_Control),
       Pause       => (Pause,                                                   Key_Special),
       Scroll_Lock => (Scroll_Lock,                                             Key_Special),
       Sys_Req     => (Sys_Req,                                                 Key_Special),
       16#FF1B#    => (Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC)), Key_Control),
       others      => (Unknown_Symbol,                                          Key_Unknown)
      );

   -- Translation array for specials that turn into themselves; needed because
   -- the ranges are discontinuous and have holes
   type Special_2_Translation is array (Lo_Spec_2 .. Hi_Spec_2) of Key_Symbol;
   Specials_2 : constant Special_2_Translation :=
      (
       Home         => Self_Symbol,
       Left         => Self_Symbol,
       Up           => Self_Symbol,
       Right        => Self_Symbol,
       Down         => Self_Symbol,
       Page_Up      => Self_Symbol,
       Page_Down    => Self_Symbol,
       End_Key      => Self_Symbol,
       Begin_Key    => Self_Symbol,
       Select_Key   => Self_Symbol,
       Print        => Self_Symbol,
       Execute      => Self_Symbol,
       Insert       => Self_Symbol,
       Undo         => Self_Symbol,
       Redo         => Self_Symbol,
       Menu         => Self_Symbol,
       Find         => Self_Symbol,
       Cancel       => Self_Symbol,
       Help         => Self_Symbol,
       Break        => Self_Symbol,
       Mode_Switch  => Self_Symbol,
       Num_Lock     => Self_Symbol,
       KP_Space     => Self_Symbol,
       KP_Tab       => Self_Symbol,
       KP_Enter     => Self_Symbol,
       KP_F1        => Self_Symbol,
       KP_F2        => Self_Symbol,
       KP_F3        => Self_Symbol,
       KP_F4        => Self_Symbol,
       KP_Home      => Self_Symbol,
       KP_Left      => Self_Symbol,
       KP_Up        => Self_Symbol,
       KP_Right     => Self_Symbol,
       KP_Down      => Self_Symbol,
       KP_Page_Up   => Self_Symbol,
       KP_Page_Down => Self_Symbol,
       KP_End       => Self_Symbol,
       KP_Begin     => Self_Symbol,
       KP_Insert    => Self_Symbol,
       KP_Delete    => Self_Symbol,
       KP_Equal     => Self_Symbol,
       KP_Multiply  => Self_Symbol,
       KP_Add       => Self_Symbol,
       KP_Separator => Self_Symbol,
       KP_Subtract  => Self_Symbol,
       KP_Decimal   => Self_Symbol,
       KP_Divide    => Self_Symbol,
       KP_0         => Self_Symbol,
       KP_1         => Self_Symbol,
       KP_2         => Self_Symbol,
       KP_3         => Self_Symbol,
       KP_4         => Self_Symbol,
       KP_5         => Self_Symbol,
       KP_6         => Self_Symbol,
       KP_7         => Self_Symbol,
       KP_8         => Self_Symbol,
       KP_9         => Self_Symbol,
       others       => Unknown_Symbol
      );

   -- Translate an X11 keysym to a Lumen symbol and a category.  The keysym
   -- comes in as a Key_Symbol type, but it's not really one just yet
   procedure Keysym_To_Symbol (Incoming  : in     Key_Symbol;
                               Modifiers : in     Modifier_Set;
                               Outgoing  :    out Key_Symbol;
                               Category  :    out Key_Category) is
   begin  -- Keysym_To_Symbol

      -- Modifiers, like shift, ctrl, alt, etc.
      if Incoming in Lo_Mod .. Hi_Mod then
         Outgoing := Incoming;
         Category := Key_Modifier;

      -- Function keys
      elsif Incoming in Lo_Func .. Hi_Func then
         Outgoing := Incoming;
         Category := Key_Function;

      -- First group of specials; some actually get translated
      elsif Incoming in Lo_Spec_1 .. Hi_Spec_1 then
         if Specials_1 (Incoming).Val = Unknown_Symbol then
            Outgoing := Incoming;  -- just retain its X11 value, whatever it means; maybe the user will know
            Category := Key_Unknown;
         else
            Outgoing := Specials_1 (Incoming).Val;
            Category := Specials_1 (Incoming).Cat;
         end if;

      -- Second group of specials, which all retain their values
      elsif Incoming in Lo_Spec_2 .. Hi_Spec_2 then
         Outgoing := Incoming;
         if Specials_2 (Incoming) = Unknown_Symbol then
            Category := Key_Unknown;
         else
            Category := Key_Special;
         end if;

      -- Finally, deal with straight Latin-1 characters; shouldn't get here
      -- because we catch them in Events, but just in case
      elsif Incoming in Lo_Char .. Hi_Char then
         Outgoing := Incoming;
         if Incoming < Lo_Graphic or Incoming = Character'Pos (Ada.Characters.Latin_1.DEL) then
            Category := Key_Control;
         else
            Category := Key_Graphic;
         end if;

      -- Any keysym that doesn't meet the above specifications
      else
         Outgoing := Incoming;
         Category := Key_Unknown;
      end if;
   end Keysym_To_Symbol;

end Lumen.Events.Key_Translate;
