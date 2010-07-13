
-- Lumen.Events.Keys -- Translated key symbol names for key press and release events
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


package Lumen.Events.Keys is

   -- Modifiers
   Shift_L      : constant Key_Symbol := 16#FFE1#;  -- Left shift
   Shift_R      : constant Key_Symbol := 16#FFE2#;  -- Right shift
   Control_L    : constant Key_Symbol := 16#FFE3#;  -- Left control
   Control_R    : constant Key_Symbol := 16#FFE4#;  -- Right control
   Caps_Lock    : constant Key_Symbol := 16#FFE5#;  -- Caps lock
   Shift_Lock   : constant Key_Symbol := 16#FFE6#;  -- Shift lock

   Meta_L       : constant Key_Symbol := 16#FFE7#;  -- Left meta
   Meta_R       : constant Key_Symbol := 16#FFE8#;  -- Right meta
   Alt_L        : constant Key_Symbol := 16#FFE9#;  -- Left alt
   Alt_R        : constant Key_Symbol := 16#FFEA#;  -- Right alt
   Super_L      : constant Key_Symbol := 16#FFEB#;  -- Left super
   Super_R      : constant Key_Symbol := 16#FFEC#;  -- Right super
   Hyper_L      : constant Key_Symbol := 16#FFED#;  -- Left hyper
   Hyper_R      : constant Key_Symbol := 16#FFEE#;  -- Right hyper

   -- Function keys
   F1           : constant Key_Symbol := 16#FFBE#;
   F2           : constant Key_Symbol := 16#FFBF#;
   F3           : constant Key_Symbol := 16#FFC0#;
   F4           : constant Key_Symbol := 16#FFC1#;
   F5           : constant Key_Symbol := 16#FFC2#;
   F6           : constant Key_Symbol := 16#FFC3#;
   F7           : constant Key_Symbol := 16#FFC4#;
   F8           : constant Key_Symbol := 16#FFC5#;
   F9           : constant Key_Symbol := 16#FFC6#;
   F10          : constant Key_Symbol := 16#FFC7#;
   F11          : constant Key_Symbol := 16#FFC8#;
   F12          : constant Key_Symbol := 16#FFC9#;
   F13          : constant Key_Symbol := 16#FFCA#;
   F14          : constant Key_Symbol := 16#FFCB#;
   F15          : constant Key_Symbol := 16#FFCC#;
   F16          : constant Key_Symbol := 16#FFCD#;
   F17          : constant Key_Symbol := 16#FFCE#;
   F18          : constant Key_Symbol := 16#FFCF#;
   F19          : constant Key_Symbol := 16#FFD0#;
   F20          : constant Key_Symbol := 16#FFD1#;
   F21          : constant Key_Symbol := 16#FFD2#;
   F22          : constant Key_Symbol := 16#FFD3#;
   F23          : constant Key_Symbol := 16#FFD4#;
   F24          : constant Key_Symbol := 16#FFD5#;
   F25          : constant Key_Symbol := 16#FFD6#;
   F26          : constant Key_Symbol := 16#FFD7#;
   F27          : constant Key_Symbol := 16#FFD8#;
   F28          : constant Key_Symbol := 16#FFD9#;
   F29          : constant Key_Symbol := 16#FFDA#;
   F30          : constant Key_Symbol := 16#FFDB#;
   F31          : constant Key_Symbol := 16#FFDC#;
   F32          : constant Key_Symbol := 16#FFDD#;
   F33          : constant Key_Symbol := 16#FFDE#;
   F34          : constant Key_Symbol := 16#FFDF#;
   F35          : constant Key_Symbol := 16#FFE0#;

   -- Specials
   Clear        : constant Key_Symbol := 16#FF0B#;
   Pause        : constant Key_Symbol := 16#FF13#;  -- Pause, hold
   Scroll_Lock  : constant Key_Symbol := 16#FF14#;
   Sys_Req      : constant Key_Symbol := 16#FF15#;

   Home         : constant Key_Symbol := 16#FF50#;
   Left         : constant Key_Symbol := 16#FF51#;  -- Move left, left arrow
   Up           : constant Key_Symbol := 16#FF52#;  -- Move up, up arrow
   Right        : constant Key_Symbol := 16#FF53#;  -- Move right, right arrow
   Down         : constant Key_Symbol := 16#FF54#;  -- Move down, down arrow
   Prior        : constant Key_Symbol := 16#FF55#;  -- Prior, previous
   Page_Up      : constant Key_Symbol := 16#FF55#;
   Next         : constant Key_Symbol := 16#FF56#;  -- Next
   Page_Down    : constant Key_Symbol := 16#FF56#;
   End_Key      : constant Key_Symbol := 16#FF57#;  -- EOL
   Begin_Key    : constant Key_Symbol := 16#FF58#;  -- BOL

   Select_Key   : constant Key_Symbol := 16#FF60#;  -- Select, mark
   Print        : constant Key_Symbol := 16#FF61#;
   Execute      : constant Key_Symbol := 16#FF62#;  -- Execute, run, do
   Insert       : constant Key_Symbol := 16#FF63#;  -- Insert, insert here
   Undo         : constant Key_Symbol := 16#FF65#;
   Redo         : constant Key_Symbol := 16#FF66#;  -- Redo, again
   Menu         : constant Key_Symbol := 16#FF67#;
   Find         : constant Key_Symbol := 16#FF68#;  -- Find, search
   Cancel       : constant Key_Symbol := 16#FF69#;  -- Cancel, stop, abort, exit
   Help         : constant Key_Symbol := 16#FF6A#;  -- Help
   Break        : constant Key_Symbol := 16#FF6B#;
   Mode_Switch  : constant Key_Symbol := 16#FF7E#;  -- Character set switch
   Num_Lock     : constant Key_Symbol := 16#FF7F#;

   KP_Space     : constant Key_Symbol := 16#FF80#;  -- Space
   KP_Tab       : constant Key_Symbol := 16#FF89#;
   KP_Enter     : constant Key_Symbol := 16#FF8D#;  -- Enter
   KP_F1        : constant Key_Symbol := 16#FF91#;  -- PF1, KP_A, ...
   KP_F2        : constant Key_Symbol := 16#FF92#;
   KP_F3        : constant Key_Symbol := 16#FF93#;
   KP_F4        : constant Key_Symbol := 16#FF94#;
   KP_Home      : constant Key_Symbol := 16#FF95#;
   KP_Left      : constant Key_Symbol := 16#FF96#;
   KP_Up        : constant Key_Symbol := 16#FF97#;
   KP_Right     : constant Key_Symbol := 16#FF98#;
   KP_Down      : constant Key_Symbol := 16#FF99#;
   KP_Prior     : constant Key_Symbol := 16#FF9A#;
   KP_Page_Up   : constant Key_Symbol := 16#FF9A#;
   KP_Next      : constant Key_Symbol := 16#FF9B#;
   KP_Page_Down : constant Key_Symbol := 16#FF9B#;
   KP_End       : constant Key_Symbol := 16#FF9C#;
   KP_Begin     : constant Key_Symbol := 16#FF9D#;
   KP_Insert    : constant Key_Symbol := 16#FF9E#;
   KP_Delete    : constant Key_Symbol := 16#FF9F#;
   KP_Equal     : constant Key_Symbol := 16#FFBD#;  -- Equals;  NOTE: this is really the last KP key!
   KP_Multiply  : constant Key_Symbol := 16#FFAA#;
   KP_Add       : constant Key_Symbol := 16#FFAB#;
   KP_Separator : constant Key_Symbol := 16#FFAC#;  -- Separator, often comma
   KP_Subtract  : constant Key_Symbol := 16#FFAD#;
   KP_Decimal   : constant Key_Symbol := 16#FFAE#;
   KP_Divide    : constant Key_Symbol := 16#FFAF#;

   KP_0         : constant Key_Symbol := 16#FFB0#;
   KP_1         : constant Key_Symbol := 16#FFB1#;
   KP_2         : constant Key_Symbol := 16#FFB2#;
   KP_3         : constant Key_Symbol := 16#FFB3#;
   KP_4         : constant Key_Symbol := 16#FFB4#;
   KP_5         : constant Key_Symbol := 16#FFB5#;
   KP_6         : constant Key_Symbol := 16#FFB6#;
   KP_7         : constant Key_Symbol := 16#FFB7#;
   KP_8         : constant Key_Symbol := 16#FFB8#;
   KP_9         : constant Key_Symbol := 16#FFB9#;

end Lumen.Events.Keys;
