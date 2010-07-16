
-- Lumen.Internal -- Internal declarations not intended for user applications
--
-- Chip Richards, NiEstu, Phoenix AZ, Spring 2010

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


package body Lumen.Internal is

   ---------------------------------------------------------------------------

   -- Simple binary semaphore
   protected body Signal is

      -- Indicate that a shutdown has occurred
      procedure Set is
      begin  -- Set
         Is_Set := true;
      end Set;

      -- Block until the signal is set, then clear it
      entry Wait when Is_Set is
      begin  -- Wait
         Is_Set := false;
      end Wait;

   end Signal;

   ---------------------------------------------------------------------------

end Lumen.Internal;
