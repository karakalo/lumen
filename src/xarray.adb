
-- XArray -- Generic extensible-array package

-- This package provides an array of the given type, whose size can be
-- extended indefinitely, up to available heap.  Probably better to use
-- Ada.Containers, though.


-- Environment
with Ada.Unchecked_Deallocation;


package body XArray is

   ---------------------------------------------------------------------------

   -- Create a new extensible array
   function Create return Pointer is
   begin  -- Create
      return new Node (Increment);  -- starts out as one chunk
   end Create;

   ---------------------------------------------------------------------------

   -- Append an element to the array.  Always use this to add new elements to
   -- the end of an array.  Assignment to existing elements directly is okay.
   procedure Append (Handle : in out Pointer;
                     Item   : in     Data_Type) is

      procedure Free is new Ada.Unchecked_Deallocation (Node, Pointer);

      Extended : Pointer;

   begin  -- Append

      -- See if current array allocation is full, and if so, extend it by
      -- allocating a new chunk, copying the old array to it, and throwing the
      -- old array away
      if Handle.Current >= Handle.Size then

         Extended := new Node (Handle.Size + Increment);

         for Copy in Handle.Data'Range loop
            Extended.Data (Copy) := Handle.Data (Copy);
         end loop;

         Extended.Current := Handle.Current;

         Free (Handle);

         Handle := Extended;
      end if;

      -- Add the new item, now that we know we have enough room
      Handle.Current := Handle.Current + 1;
      Handle.Data (Handle.Current) := Item;
   end Append;

   ---------------------------------------------------------------------------

   -- Helper procedure to remove an element from the array.
   procedure Delete (Handle : in Pointer;
                     Index  : in Positive) is
   begin  -- Delete

      -- Ignore the deletion if the index is out of range
      if Index not in Handle.Data'First .. Handle.Current then
         return;
      end if ;

      -- Copy all elements after the deleted one down one slot.  Slow but
      -- simple.
      for Move in Index + 1 .. Handle.Current loop
         Handle.Data (Move - 1) := Handle.Data (Move);
      end loop;

      -- Note that we removed it
      Handle.Current := Handle.Current - 1;

   end Delete;

   ---------------------------------------------------------------------------

   -- Helper function to look up an element in the array, returns the found
   -- element's index, or zero meaning "not found".
   function Find (Handle : in Pointer;
                  Item   : in Data_Type)
   return Natural is
   begin  -- Find

      -- Search for the item in the array, and return its index if found.
      -- Linear search, slow but simple.
      for Scan in Handle.Data'First .. Handle.Current loop
         if Handle.Data (Scan) = Item then
            return Scan;
         end if;
      end loop;

      -- No find, return zero
      return 0;
   end Find;

   ---------------------------------------------------------------------------

end XArray;
