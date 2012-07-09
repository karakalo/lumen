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

pragma Ada_2005;

package Win32.GDI32 is

   function ChoosePixelFormat
     (hdc  : HDC_Type;
      ppfd : access PIXELFORMATDESCRIPTOR_Type)
      return Interfaces.C.int;
   pragma Import(StdCall,ChoosePixelFormat,"ChoosePixelFormat");

   function SetPixelFormat
     (hdc          : HDC_Type;
      iPixelFormat : Interfaces.C.int;
      ppfd         : access PIXELFORMATDESCRIPTOR_Type)
      return BOOL_Type;
   pragma Import(StdCall,SetPixelFormat,"SetPixelFormat");

end Win32.GDI32;
