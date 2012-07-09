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

pragma Ada_2005;

package body Win32 is

   function MAKEINTRESOURCE
     (wInteger : WORD_Type)
      return LPCTSTR_Type is

      Value : LPCTSTR_Type
        :=Interfaces.C.Strings.Null_Ptr;

      pragma Warnings(Off);
      function Convert is new Ada.Unchecked_Conversion
        (Source => Word_Type,
         Target => LPCTSTR_Type);
      pragma Warnings(On);

   begin
      Value := Convert(wInteger);
      return Value;
   end MAKEINTRESOURCE;
   ---------------------------------------------------------------------------

   function GET_X_LPARAM
     (lParam : LPARAM_Type)
      return Integer is

      pragma Warnings(Off);
      function Convert is new Ada.Unchecked_Conversion
        (Source => LPARAM_Type,
         Target => Interfaces.Integer_16);
      pragma Warnings(On);

   begin
      return Integer(Convert(lparam));
   end GET_X_LPARAM;
   ---------------------------------------------------------------------------

   function GET_Y_LPARAM
     (lParam : LPARAM_Type)
      return Integer is

      type Words is
         record
            LoWord : Interfaces.Integer_16;
            HiWord : Interfaces.Integer_16;
         end record;
      pragma Convention(C,Words);

      pragma Warnings(Off);
      function Convert is new Ada.Unchecked_Conversion
        (Source => LPARAM_Type,
         Target => Words);
      pragma Warnings(On);

   begin
      return Integer(Convert(lparam).HiWord);
   end GET_Y_LPARAM;
   ---------------------------------------------------------------------------

   function LOWORD
     (lParam : LPARAM_Type)
      return WORD_Type is

      pragma Warnings(Off);
      function Convert is new Ada.Unchecked_Conversion
        (Source => LPARAM_Type,
         Target => WORD_Type);
      pragma Warnings(On);

   begin
      return Convert(lparam);
   end LOWORD;
   ---------------------------------------------------------------------------

   function HIWORD
     (lParam : LPARAM_Type)
      return WORD_Type is

      type Words is
         record
            LoWord : WORD_Type;
            HiWord : WORD_Type;
         end record;
      pragma Convention(C,Words);

      pragma Warnings(Off);
      function Convert is new Ada.Unchecked_Conversion
        (Source => LPARAM_Type,
         Target => Words);
      pragma Warnings(On);

   begin
      return Convert(lparam).HiWord;
   end HIWORD;
   ---------------------------------------------------------------------------

end Win32;
