
-- Lumen.Program -- Helper routines fr working with shader programs.
--
-- darkestkhan, Winter 2014

-- This code is covered by the ISC License:
--
-- Copyright © 2014, darkestkhan
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

with Interfaces.C;

with Lumen.Binary;

use type Lumen.Binary.Byte;

package body Lumen.Program is

   ---------------------------------------------------------------------------

   function Get_Info_Log (Program : in GL.UInt) return String is

      Log_Len : GL.Int;

   begin  -- Get_Info_Log

      GL.Get_Program (Program, GL.GL_INFO_LOG_LENGTH, Log_Len'Address);

      declare
         Log : Interfaces.C.char_array (1 .. Interfaces.C.size_t (Log_Len));
         Got : GL.SizeI;
      begin
         GL.Get_Program_Info_Log (Program,
                                  Log'Length,
                                  Got'Address,
                                  Log'Address);
         return Interfaces.C.To_Ada (Log);
      end;

   end Get_Info_Log;

   ---------------------------------------------------------------------------

end Lumen.Program;
