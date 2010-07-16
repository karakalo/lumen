
-- XArray -- Generic extensible-array package


-- This package provides an array of the given type, whose size can be
-- extended indefinitely, up to available heap.  Probably better to use
-- Ada.Containers, though.
--
-- Increment is purely a tuning constant.  It's the size of chunks by which
-- the array is allocated.  Each time you add this many new items plus one,
-- the array is extended by this much.

generic
   type Data_Type is private;
   Increment : in Positive := 1024;

package XArray is
   pragma Preelaborate;

   -- The array type, the extensible-array record, and the pointer type that
   -- users use to get at it
   type XArray_Type is array (Positive range <>) of Data_Type;
   type Node (Size : Positive) is record
      Current : Natural := 0;
      Data    : XArray_Type (1 .. Size);
   end record;
   type Pointer is access Node;

   -- Create a new extensible array
   function Create return Pointer;

   -- Append an element to the array.  Always use this to add new elements to
   -- the end of an array.  Assignment to existing elements directly is okay.
   procedure Append (Handle : in out Pointer;
                     Item   : in     Data_Type);

   -- Helper procedure to remove an element from the array.
   procedure Delete (Handle : in Pointer;
                     Index  : in Positive);

   -- Helper function to look up an element in the array, returns the found
   -- element's index, or zero meaning "not found".
   function Find (Handle : in Pointer;
                  Item   : in Data_Type)
   return Natural;

   ---------------------------------------------------------------------------

end XArray;
