
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


package Lumen.Events.Key_Translate is

   -- Translate an X11 keysym to a Lumen symbol and a category.  The keysym
   -- comes in as a Key_Symbol type, but it's not really one just yet
   procedure Keysym_To_Symbol (Incoming  : in     Key_Symbol;
                               Modifiers : in     Modifier_Set;
                               Outgoing  :    out Key_Symbol;
                               Category  :    out Key_Category);

end Lumen.Events.Key_Translate;
