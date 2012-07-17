-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-- Revision History
--   18.Mar 2012 Julian Schutsch
--     - Original version

package Win32.OpenGL32 is

   function wglCreateContext
     (hdc : HDC_Type)
      return HGLRC_Type;
   pragma Import(StdCall,wglCreateContext,"wglCreateContext");

   function wglMakeCurrent
     (hdc : HDC_Type;
      hglrc : HGLRC_Type)
      return BOOL_Type;
   pragma Import(StdCall,wglMakeCurrent,"wglMakeCurrent");

   function wglGetProcAddress
     (ProcName : Interfaces.C.Strings.chars_ptr)
      return System.Address;
   pragma Import(StdCall,wglGetProcAddress,"wglGetProcAddress");

   function SwapBuffers
     (hdc : HDC_Type)
      return BOOL_Type;
   pragma Import(StdCall,SwapBuffers,"SwapBuffers");

end Win32.OpenGL32;
