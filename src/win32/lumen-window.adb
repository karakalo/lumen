-- Win32 version

with Ada.Unchecked_Deallocation;
package body Lumen.Window is

   type Win32Window_Type is new Window_Type with
      record
         null;
      end record;

   procedure Create (Win           : in out Window_Handle;
                     Parent        : in     Window_Handle      := No_Window;
                     Width         : in     Natural            := 400;
                     Height        : in     Natural            := 400;
                     Events        : in     Wanted_Event_Set   := Want_No_Events;
                     Name          : in     String             := "";
                     Icon_Name     : in     String             := "";
                     Class_Name    : in     String             := "";
                     Instance_Name : in     String             := "";
                     Context       : in     Context_Handle     := No_Context;
                     Depth         : in     Color_Depth        := True_Color;
                     Direct        : in     Boolean            := True;
                     Animated      : in     Boolean            := True;
                     Attributes    : in     Context_Attributes := Default_Context_Attributes) is
   begin
      Win:=new Win32Window_Type;
   end Create;

   ---------------------------------------------------------------------------

   procedure Destroy (Win : in out Window_Handle) is
      procedure Free is new Ada.Unchecked_Deallocation(Window_Type'Class,Window_Handle);
   begin
      Free(Win);
   end Destroy;

   ---------------------------------------------------------------------------

   procedure Set_Names (Win           : in Window_Handle;
                        Name          : in String           := "";
                        Icon_Name     : in String           := "";
                        Class_Name    : in String           := "";
                        Instance_Name : in String           := "") is
   begin
      null;
   end Set_Names;

   ---------------------------------------------------------------------------

   function Create_Context (Win    : in Window_Handle;
                            Direct : in Boolean := True)
   return Context_Handle is

   begin  -- Create_Context
--      return GLX_Create_Context (Win.Display, Win.Visual, GLX_Context (System.Null_Address),
          --                                 Character'Val (Boolean'Pos (Direct)));
      return null;
   end Create_Context;

   ---------------------------------------------------------------------------

   procedure Destroy_Context (Win : in out Window_Handle) is
   begin
      null;
   end Destroy_Context;

   ---------------------------------------------------------------------------

   procedure Make_Current (Win : in Window_Handle) is
   begin  -- Make_Current
      null;
   end Make_Current;

   ---------------------------------------------------------------------------

   -- Make a rendering context the current one for a window
   procedure Set_Context (Win     : in out Window_Handle;
                          Context : in     Context_Handle) is
   begin  -- Set_Context
      null;
   end Set_Context;

   ---------------------------------------------------------------------------

   -- Promotes the back buffer to front; only valid if the window is double
   -- buffered, meaning Animated was true when the window was created.  Useful
   -- for smooth animation.
   procedure Swap (Win : in Window_Handle) is

   begin  -- Swap
      null;
   end Swap;

   ---------------------------------------------------------------------------

   -- Return current window width
   function Width (Win : in Window_Handle) return Natural is
   begin  -- Width
      return Win.Width;
   end Width;

   ---------------------------------------------------------------------------

   -- Return current window width
   function Height (Win : in Window_Handle) return Natural is
   begin  -- Height
      return Win.Height;
   end Height;

   ---------------------------------------------------------------------------

end Lumen.Window;
