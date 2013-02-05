
--
-- PQueue -- Generic protected queue package
--


-- This code was adapted from two different chunks of code in Norman
-- H. Cohen's excellent book Ada as a Second Language.  It implements a simple
-- protected generic queue type.

generic
   type Data_Type is private;

package PQueue is
pragma Preelaborate;

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- The implementation details of the queue aren't important to users
   type Queue_Type is limited private;

   -- The protected type, providing an interface to manage the queue
   protected type Protected_Queue_Type is

      -- Place an item on the back of the queue.  Doesn't block.
      procedure Enqueue (Item : in  Data_Type);

      -- Remove an item from the front of the queue.  Blocks if no items are
      -- present.
      entry     Dequeue (Item : out Data_Type);

      -- Return the number of items presently on the queue.  Doesn't block.
      function  Length return natural;

   private
      -- The actual queue we're protecting
      Queue : Queue_Type;
   end Protected_Queue_Type;

------------------------------------------------------------------------------
--
-- Private types
--
------------------------------------------------------------------------------

private

   -- Implementation of a simple queue, using a linked list.  First, define
   -- the list node type, with a data item of whatever type the user specifies
   -- when they instantiate this generic package.
   type Queue_Element_Type;
   type Queue_Element_Pointer is access Queue_Element_Type;
   type Queue_Element_Type is record
      Data : Data_Type;
      Next : Queue_Element_Pointer;
   end record;

   -- Now the queue itself, with its length, and pointers to the front and
   -- back items.
   type Queue_Type is record
      Len   : natural := 0;
      Front : Queue_Element_Pointer := null;
      Back  : Queue_Element_Pointer := null;
   end record;

   ---------------------------------------------------------------------------

end PQueue;
