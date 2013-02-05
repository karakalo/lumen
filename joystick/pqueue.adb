
--
-- PQueue -- Generic protected queue package
--


--
-- Standard packages
with Ada.Unchecked_Deallocation;


package body PQueue is

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Instantiate this so we can free storage allocated for queue items once
   -- they are removed from the queue
   procedure Free_Queue_Element is new Ada.Unchecked_Deallocation (Queue_Element_Type,
                                                                   Queue_Element_Pointer);

------------------------------------------------------------------------------
--
-- Public protected objects
--
------------------------------------------------------------------------------

   protected body Protected_Queue_Type is

      ------------------------------------------------------------------------

      -- Place an item on the back of the queue.  Doesn't block.
      procedure Enqueue (Item : in  Data_Type) is

         New_Element : Queue_Element_Pointer;

      begin  -- Enqueue

         -- Allocate a new node for the list
         New_Element := new Queue_Element_Type'(Item, null);

         -- If the queue is empty, stick it on the "front"; if not, add it to
         -- the back
         if Queue.Len = 0 then
            Queue.Front := New_Element;
         else
            Queue.Back.Next := New_Element;
         end if;

         -- Whatever our previous status, the queue now has a new item on its
         -- back end
         Queue.Back := New_Element;
         Queue.Len := Queue.Len + 1;
      end Enqueue;

      ------------------------------------------------------------------------

      -- Remove an item from the front of the queue.  Blocks if no items are
      -- present, accomplished by a guard condition on the entry.
      entry     Dequeue (Item : out Data_Type) when Queue.Len > 0 is

         Old_Front : Queue_Element_Pointer;

      begin  -- Dequeue

         -- Once the guard lets us get here, there's at least one item on the
         -- queue; remove it by advancing the "front" pointer (which may thus
         -- become null)
         Old_Front := Queue.Front;
         Queue.Front := Queue.Front.Next;

         -- Return the dequeued item to the caller
         Item := Old_Front.Data;

         -- Free the list node now that we've copied its data, and decrement
         -- the item count
         Free_Queue_Element (Old_Front);
         Queue.Len := Queue.Len - 1;
      end Dequeue;

      ------------------------------------------------------------------------

      -- Return the number of items presently on the queue.  Doesn't block.
      function  Length return natural is
      begin  -- Length
         return Queue.Len;
      end Length;

      ------------------------------------------------------------------------

   end Protected_Queue_Type;

   ---------------------------------------------------------------------------

end PQueue;
