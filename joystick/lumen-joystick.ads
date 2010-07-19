
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

   ---------------------------------------------------------------------------

   -- Exceptions defined by this package
   Open_Failed  : exception;  -- cannot open given joystick device, or cannot fetch its info

   -- The default joystick device pathname
   Default_Pathname : String := "/dev/input/js0";

   ---------------------------------------------------------------------------

   -- Joystick event
   type Joystick_Event_Type is (Joystick_Axis_Change, Joystick_Button_Press, Joystick_Button_Release);
   type Joystick_Event_Data (Which : Joystick_Event_Type := Joystick_Axis_Change) is record
      Number : Positive;  -- which axis or button
      case Which is
         when Joystick_Axis_Change =>
            Axis_Value : Integer;
         when Joystick_Button_Press | Joystick_Button_Release =>
            null;
      end case;
   end record;

   ---------------------------------------------------------------------------

   -- Our joystick handle, used to refer to joysticks within the library and app
   type Stick_Info_Pointer is private;
   type Handle is new Ada.Finalization.Limited_Controlled with record
      Info : Stick_Info_Pointer;
   end record;
   procedure Finalize (Stick : in out Handle);

   ---------------------------------------------------------------------------

   -- Open a joystick device
   procedure Open (Stick : in out Handle;
                   Path  : in     String := Default_Pathname);

   -- Close a joystick device
   procedure Close (Stick : in out Handle);

   -- Various joystick information functions
   function Name    (Stick : in Handle) return String;    -- joystick's name
   function Axes    (Stick : in Handle) return Natural;   -- number of axes
   function Buttons (Stick : in Handle) return Natural;   -- number of buttons

   -- Returns the number of events that are waiting in the joystick's event
   -- queue.  Useful to avoid blocking while waiting for the next event to
   -- show up.
   function Pending (Stick : in Handle) return Natural;

   -- Read a joystick event
   function Next_Event (Stick : in Handle) return Joystick_Event_Data;

   ---------------------------------------------------------------------------

private

   type Stick_Info;
   type Stick_Info_Pointer is access Stick_Info;

end Lumen.Joystick;
