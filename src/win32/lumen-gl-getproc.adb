with Interfaces.C.Strings;
with Win32.OpenGL32;

package body Lumen.GL.GetProc is

   function GetProcAddress
     (Name : String)
      return System.Address is
      CName : Interfaces.C.Strings.chars_ptr;
      Result : System.Address;
   begin

      CName:=Interfaces.C.Strings.New_String(Name);
      Result:=Win32.OpenGL32.wglGetProcAddress(CName);
      Interfaces.C.Strings.Free(CName);
      return Result;

   end GetProcAddress;
   ---------------------------------------------------------------------------

end Lumen.GL.GetProc;
