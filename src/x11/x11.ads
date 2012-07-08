
with System;
with Lumen; use Lumen;

package X11 is

   -- Xlib stuff needed by more than one of the routines below
   type Data_Format_Type is (Invalid, Bits_8, Bits_16, Bits_32);
   for  Data_Format_Type use (Invalid => 0, Bits_8 => 8, Bits_16 => 16, Bits_32 => 32);

   type Atom            is new Long_Integer;

   -- Values used to compute record rep clause values that are portable
   -- between 32- and 64-bit systems
   Is_32      : constant := Boolean'Pos (System.Word_Size = 32);
   Is_64      : constant := 1 - Is_32;
   Word_Bytes : constant := Integer'Size / System.Storage_Unit;
   Word_Bits  : constant := Integer'Size - 1;
   Long_Bytes : constant := Long_Integer'Size / System.Storage_Unit;
   Long_Bits  : constant := Long_Integer'Size - 1;

   -- Xlib types needed only by Create
   subtype Dimension is Short_Integer;
   subtype Pixel     is Long_Integer;
   subtype Position  is Short_Integer;

   type Int_Ptr is access all Integer;  -- used to simulate "out" param for C function
   type FB_Config_Ptr is access all System.Address;  -- actually an array, but only ever one element

   Max_GLX_Attributes : constant := (Context_Attribute_Name'Pos (Context_Attribute_Name'Last) + 1) * 2 + 2;

   type GLX_Attribute_List     is array (1 .. Max_GLX_Attributes) of Integer;
   type GLX_Attribute_List_Ptr is new System.Address;

   ------------------------------------------------------------------------

   type Display_Pointer is new System.Address;

   -- The maximum length of an event data record
   type Padding is array (1 .. 23) of Long_Integer;

   type Screen_Depth    is new Natural;
   type Screen_Number   is new Natural;
   type Visual_ID       is new Long_Integer;
   type Window_ID       is new Long_Integer;

   type Alloc_Mode               is (Alloc_None, Alloc_All);
   type Atom_Array               is array (Positive range <>) of Atom;
   type Colormap_ID              is new Long_Integer;
   type X_Window_Attributes_Mask is mod 2 ** Integer'Size;
   type Window_Class             is (Copy_From_Parent, Input_Output, Input_Only);
   type X_Event_Mask             is mod 2 ** Long_Integer'Size;

   -- An extremely abbreviated version of the XSetWindowAttributes
   -- structure, containing only the fields we care about.
   --
   -- NOTE: offset multiplier values differ between 32-bit and 64-bit
   -- systems since on 32-bit systems long size equals int size and the
   -- record has no padding.  The byte and bit widths come from Internal.
   Start_32 : constant := 10;
   Start_64 : constant :=  9;
   Start    : constant := (Is_32 * Start_32) + (Is_64 * Start_64);
   ------------------------------------------------------------------------

   Null_Display_Pointer : constant Display_Pointer := Display_Pointer (System.Null_Address);

   type X_Set_Window_Attributes is record
      Event_Mask  : X_Event_Mask := 0;
      Colormap    : Colormap_ID  := 0;
   end record;
   for X_Set_Window_Attributes use record
      Event_Mask  at (Start + 0) * Long_Bytes range 0 .. Long_Bits;
      Colormap    at (Start + 3) * Long_Bytes range 0 .. Long_Bits;
   end record;

   -- The GL rendering context type
   type GLX_Context is new System.Address;

   type X_Visual_Info is record
      Visual        : System.Address;
      Visual_Ident  : Visual_ID;
      Screen        : Screen_Number;
      Depth         : Screen_Depth;
      Class         : Integer;
      Red_Mask      : Long_Integer;
      Green_Mask    : Long_Integer;
      Blue_Mask     : Long_Integer;
      Colormap_Size : Natural;
      Bits_Per_RGB  : Natural;
   end record;
   type X_Visual_Info_Pointer is access all X_Visual_Info;

   type X_Class_Hint is record
      Instance_Name : System.Address;
      Class_Name    : System.Address;
   end record;

   type X_Text_Property is record
      Value    : System.Address;
      Encoding : Atom;
      Format   : Data_Format_Type;
      NItems   : Long_Integer;
   end record;

   ---------------------------------------------------------------------------

   -- X modifier mask and its values
   type Modifier_Mask is mod 2 ** Integer'Size;

   -- Xlib constants needed only by Create
   Configure_Event_Mask : constant X_Window_Attributes_Mask := 2#00_1000_0000_0000#;  -- 11th bit
   Configure_Colormap   : constant X_Window_Attributes_Mask := 2#10_0000_0000_0000#;  -- 13th bit

   -- Atom names
   WM_Del          : String := "WM_DELETE_WINDOW" & ASCII.NUL;

   Null_Context : constant GLX_Context := GLX_Context (System.Null_Address);

   -- X event type codes
   X_Error            : constant :=  0;  -- we don't actually use this, just there to define bounds
   X_Key_Press        : constant :=  2;
   X_Key_Release      : constant :=  3;
   X_Button_Press     : constant :=  4;
   X_Button_Release   : constant :=  5;
   X_Motion_Notify    : constant :=  6;
   X_Enter_Notify     : constant :=  7;
   X_Leave_Notify     : constant :=  8;
   X_Focus_In         : constant :=  9;
   X_Focus_Out        : constant := 10;
   X_Expose           : constant := 12;
   X_Unmap_Notify     : constant := 18;
   X_Map_Notify       : constant := 19;
   X_Configure_Notify : constant := 22;
   X_Client_Message   : constant := 33;
   X_Generic_Event    : constant := 35;  -- we don't actually use this, just there to define bounds

   X_First_Event    : constant := X_Error;
   X_Last_Event     : constant := X_Generic_Event + 1;

   -- Our "delete window" atom value
   Delete_Window_Atom : Atom;
   ---------------------------------------------------------------------------

   -- Xlib functions needed only by Create
   function X_Create_Colormap (Display : Display_Pointer;
                               Window  : Window_ID;
                               Visual  : System.Address;
                               Alloc   : Alloc_Mode)
                                  return Colormap_ID;
   pragma Import (C, X_Create_Colormap, "XCreateColormap");

   function X_Create_Window (Display      : Display_Pointer;
                             Parent       : Window_ID;
                             X            : Position;
                             Y            : Position;
                             Width        : Dimension;
                             Height       : Dimension;
                             Border_Width : Natural;
                             Depth        : Screen_Depth;
                             Class        : Window_Class;
                             Visual       : System.Address;
                             Valuemask    : X_Window_Attributes_Mask;
                             Attributes   : System.Address)
                             return Window_ID;
   pragma Import (C, X_Create_Window, "XCreateWindow");

   function X_Default_Screen (Display : Display_Pointer) return Screen_Number;
   pragma Import (C, X_Default_Screen, "XDefaultScreen");

   procedure X_Map_Window (Display : in Display_Pointer;   Window : in Window_ID);
   pragma Import (C, X_Map_Window, "XMapWindow");

   function X_Open_Display (Display_Name : System.Address := System.Null_Address) return Display_Pointer;
   pragma Import (C, X_Open_Display, "XOpenDisplay");

   function X_Root_Window (Display : Display_Pointer;   Screen_Num : Screen_Number) return Window_ID;
   pragma Import (C, X_Root_Window, "XRootWindow");

   procedure X_Set_WM_Protocols (Display   : in Display_Pointer;
                                 Window    : in Window_ID;
                                 Protocols : in System.Address;
                                 Count     : in Integer);
   pragma Import (C, X_Set_WM_Protocols, "XSetWMProtocols");

   -- Binding to XNextEvent, used by Window for mapping notify events, and by
   -- Events for everything else
   procedure X_Next_Event (Display : in Display_Pointer;
                           Event   : in System.Address);
   pragma Import (C, X_Next_Event, "XNextEvent");

   procedure GLX_Destroy_Context (Display : in Display_Pointer;
                                  Context : GLX_Context);
   pragma Import (C, GLX_Destroy_Context, "glXDestroyContext");

   -- Binding needed locally to translate keycodes
   function X_Lookup_String (Event   : in System.Address;
                             Buffer  : in System.Address;
                             Limit   : in Natural;
                             Keysym  : in System.Address;
                             Compose : in System.Address)
                                return Natural;
   pragma Import (C, X_Lookup_String, "XLookupString");

   function X_Pending (Display : Display_Pointer) return Natural;
   pragma Import (C, X_Pending, "XPending");

   ------------------------------------------------------------------------

   Shift_Mask    : constant Modifier_Mask := 2#0000_0000_0000_0001#;
   Lock_Mask     : constant Modifier_Mask := 2#0000_0000_0000_0010#;
   Control_Mask  : constant Modifier_Mask := 2#0000_0000_0000_0100#;
   Mod_1_Mask    : constant Modifier_Mask := 2#0000_0000_0000_1000#;
   Mod_2_Mask    : constant Modifier_Mask := 2#0000_0000_0001_0000#;
   Mod_3_Mask    : constant Modifier_Mask := 2#0000_0000_0010_0000#;
   Mod_4_Mask    : constant Modifier_Mask := 2#0000_0000_0100_0000#;
   Mod_5_Mask    : constant Modifier_Mask := 2#0000_0000_1000_0000#;
   Button_1_Mask : constant Modifier_Mask := 2#0000_0001_0000_0000#;
   Button_2_Mask : constant Modifier_Mask := 2#0000_0010_0000_0000#;
   Button_3_Mask : constant Modifier_Mask := 2#0000_0100_0000_0000#;
   Button_4_Mask : constant Modifier_Mask := 2#0000_1000_0000_0000#;
   Button_5_Mask : constant Modifier_Mask := 2#0001_0000_0000_0000#;

   type X_Event_Code is new Integer range X_First_Event .. X_Last_Event;

   Bytes     : constant := Word_Bytes;
   Bits      : constant := Word_Bits;
   Atom_Bits : constant := Atom'Size - 1;
   Base_1_32 : constant :=  8;
   Base_2_32 : constant :=  5;
   Base_3_32 : constant :=  6;
   Base_4_32 : constant :=  7;
   Base_1_64 : constant := 16;
   Base_2_64 : constant := 10;
   Base_3_64 : constant := 12;
   Base_4_64 : constant := 14;
   Base_1    : constant := (Base_1_32 * Is_32) + (Base_1_64 * Is_64);
   Base_2    : constant := (Base_2_32 * Is_32) + (Base_2_64 * Is_64);
   Base_3    : constant := (Base_3_32 * Is_32) + (Base_3_64 * Is_64);
   Base_4    : constant := (Base_4_32 * Is_32) + (Base_4_64 * Is_64);

   type X_Event_Data (X_Event_Type : X_Event_Code := X_Error) is record
      case X_Event_Type is
         when X_Key_Press | X_Key_Release =>
            Key_X      : Natural;
            Key_Y      : Natural;
            Key_Root_X : Natural;
            Key_Root_Y : Natural;
            Key_State  : Modifier_Mask;
            Key_Code   : Natural;
         when X_Button_Press | X_Button_Release =>
            Btn_X      : Natural;
            Btn_Y      : Natural;
            Btn_Root_X : Natural;
            Btn_Root_Y : Natural;
            Btn_State  : Modifier_Mask;
            Btn_Code   : Natural;
         when X_Motion_Notify =>
            Mov_X      : Natural;
            Mov_Y      : Natural;
            Mov_Root_X : Natural;
            Mov_Root_Y : Natural;
            Mov_State  : Modifier_Mask;
         when X_Enter_Notify | X_Leave_Notify =>
            Xng_X      : Natural;
            Xng_Y      : Natural;
            Xng_Root_X : Natural;
            Xng_Root_Y : Natural;
         when X_Expose =>
            Xps_X      : Natural;
            Xps_Y      : Natural;
            Xps_Width  : Natural;
            Xps_Height : Natural;
            Xps_Count  : Natural;
         when X_Configure_Notify =>
            Cfg_X      : Natural;
            Cfg_Y      : Natural;
            Cfg_Width  : Natural;
            Cfg_Height : Natural;
         when X_Client_Message =>
            Msg_Value  : Atom;
         when others =>
            Pad        : Padding;
      end case;
   end record;

   for X_Event_Data use record
      X_Event_Type at  0 * Bytes range 0 .. Bits;

      Key_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Key_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Key_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Key_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;
      Key_State    at (Base_1 + 4) * Bytes range 0 .. Bits;
      Key_Code     at (Base_1 + 5) * Bytes range 0 .. Bits;

      Btn_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Btn_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Btn_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Btn_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;
      Btn_State    at (Base_1 + 4) * Bytes range 0 .. Bits;
      Btn_Code     at (Base_1 + 5) * Bytes range 0 .. Bits;

      Mov_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Mov_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Mov_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Mov_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;
      Mov_State    at (Base_1 + 4) * Bytes range 0 .. Bits;

      Xng_X        at (Base_1 + 0) * Bytes range 0 .. Bits;
      Xng_Y        at (Base_1 + 1) * Bytes range 0 .. Bits;
      Xng_Root_X   at (Base_1 + 2) * Bytes range 0 .. Bits;
      Xng_Root_Y   at (Base_1 + 3) * Bytes range 0 .. Bits;

      Xps_X        at (Base_2 + 0) * Bytes range 0 .. Bits;
      Xps_Y        at (Base_2 + 1) * Bytes range 0 .. Bits;
      Xps_Width    at (Base_2 + 2) * Bytes range 0 .. Bits;
      Xps_Height   at (Base_2 + 3) * Bytes range 0 .. Bits;

      Cfg_X        at (Base_3 + 0) * Bytes range 0 .. Bits;
      Cfg_Y        at (Base_3 + 1) * Bytes range 0 .. Bits;
      Cfg_Width    at (Base_3 + 2) * Bytes range 0 .. Bits;
      Cfg_Height   at (Base_3 + 3) * Bytes range 0 .. Bits;

      Msg_Value    at (Base_4 + 0) * Bytes range 0 .. Atom_Bits;
   end record;

   ------------------------------------------------------------------------

   GL_TRUE : constant Character := Character'Val (1);

   -- GLX stuff needed by more than one of the routines below
   function GLX_Create_Context (Display    : Display_Pointer;
                                Visual     : X_Visual_Info_Pointer;
                                Share_List : GLX_Context;
                                Direct     : Character)
   return GLX_Context;
   pragma Import (C, GLX_Create_Context, "glXCreateContext");

   function GLX_Make_Current (Display  : Display_Pointer;
                              Drawable : Window_ID;
                              Context  : GLX_Context)
   return Character;
   pragma Import (C, GLX_Make_Current, "glXMakeCurrent");

   function GLX_Make_Context_Current (Display  : Display_Pointer;
                                      Draw     : Window_ID;
                                      Read     : Window_ID;
                                      Context  : GLX_Context)
   return Character;
   pragma Import (C, GLX_Make_Context_Current, "glXMakeContextCurrent");

   ---------------------------------------------------------------------------

   function X_Intern_Atom (Display        : Display_Pointer;
                           Name           : System.Address;
                           Only_If_Exists : Natural)
   return Atom;
   pragma Import (C, X_Intern_Atom, "XInternAtom");

   procedure X_Set_Class_Hint (Display : in Display_Pointer;
                               Window  : in Window_ID;
                               Hint    : in X_Class_Hint);
   pragma Import (C, X_Set_Class_Hint, "XSetClassHint");

   procedure X_Set_Icon_Name (Display : in Display_Pointer;
                              Window  : in Window_ID;
                              Name    : in System.Address);
   pragma Import (C, X_Set_Icon_Name, "XSetIconName");

   procedure X_Set_WM_Icon_Name (Display   : in Display_Pointer;
                                 Window    : in Window_ID;
                                 Text_Prop : in System.Address);
   pragma Import (C, X_Set_WM_Icon_Name, "XSetWMIconName");

   procedure X_Set_WM_Name (Display   : in Display_Pointer;
                            Window    : in Window_ID;
                            Text_Prop : in System.Address);
   pragma Import (C, X_Set_WM_Name, "XSetWMName");

   -- GLX functions needed only by Create
   function GLX_Choose_Visual (Display        : Display_Pointer;
                               Screen         : Screen_Number;
                               Attribute_List : GLX_Attribute_List_Ptr)
                                  return X_Visual_Info_Pointer;
   pragma Import (C, GLX_Choose_Visual, "glXChooseVisual");
   ---------------------------------------------------------------------------

   function GLX_Choose_FB_Config (Display        : Display_Pointer;
                                  Screen         : Screen_Number;
                                  Attribute_List : GLX_Attribute_List_Ptr;
                                  Num_Found      : Int_Ptr)
                                        return FB_Config_Ptr;
   pragma Import (C, GLX_Choose_FB_Config, "glXChooseFBConfig");

   function GLX_Get_Visual_From_FB_Config (Display : Display_Pointer;
                                           Config  : System.Address)
                                                 return X_Visual_Info_Pointer;
   pragma Import (C, GLX_Get_Visual_From_FB_Config, "glXGetVisualFromFBConfig");

   ---------------------------------------------------------------------

end X11;
