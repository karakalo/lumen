
-- Lumen.Joystick -- Support (Linux) joysticks under Lumen
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
with Ada.Finalization;

with Lumen.Internal;
with Lumen.Window;


package Lumen.Joystick is

   -- Exceptions defined by this package
   Open_Failed  : exception;  -- cannot open given joystick device, or cannot fetch its info
   Events_Bound : exception;  -- stick events are bound to a window, so no direct events available

   -- The default joystick device pathname
   Default_Pathname : String := "/dev/input/js0";

   -- Our joystick handle, used to refer to joysticks within the library and app
   type Stick_Info_Pointer is private;
   type Handle is new Ada.Finalization.Limited_Controlled with record
      Info : Stick_Info_Pointer;
   end record;
   procedure Finalize (Stick : in out Handle);

   -- Open a joystick device, and optionally bind it to a Lumen Window
   procedure Open (Stick : in out Handle;
                   Path  : in     String := Default_Pathname;
                   Win   : in     Window.Handle := Window.No_Window);

   -- Close a joystick device
   procedure Close (Stick : in out Handle);

   -- If you don't bind a joystick to a window, then you need to read its
   -- events directly
   function Next_Event (Stick : in Handle) return Internal.Joystick_Event_Data;

private

   type Stick_Info;
   type Stick_Info_Pointer is access Stick_Info;

end Lumen.Joystick;
