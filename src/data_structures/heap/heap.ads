with Interfaces;
--
-- Generic stable maximum heap data structure which has O(log n) insertion
-- time and O(log n) removal time. The heap is stable in the sense that
-- if two objects which are placed into the heap with the same priority,
-- it follows that they will come out in the same order in which they were placed
-- into the heap (FIFO).
--
-- Items of the highest value will be popped off the heap first. Items of
-- equal value will be popped off in FIFO order. Insertion and deletion
-- from the heap are both O(log(n)) time.
--
-- To instantiate the generic heap you must define the storage element
-- type and two functions the compare "greater than" and "equal to" of
-- the type and return a boolean stating whether the condition is
-- True or False. These function are used to determine which elements
-- have higher priority and equal priority to other elements, respectively.
--
-- Note that many internal swaps and copies occur as part of pushing and
-- popping elements off a heap. Do not instantiate this generic with a large
-- data type, as you will take a performance hit for the internal copy operations.
-- If you want to store large data on a heap, consider storing
-- an id or index in this heap data structure that references a larger
-- data store that does not require swapping and copying of data.
--
generic
   type Element_Type is private;   -- Element to store on heap. Should include some sort of "priority" field
   with function ">" (Left, Right : in Element_Type) return Boolean is <>; -- Returns true if the left element if of greater priority
   with function "=" (Left, Right : in Element_Type) return Boolean is <>; -- Returns true if the elements are of equal priority
package Heap is

   -- The heap type:
   type Instance is tagged limited private;
   type Instance_Access is access all Instance;

   -- Initialization subprograms:
   procedure Init (Self : in out Instance; Maximum_Size : in Positive);
   procedure Destroy (Self : in out Instance);
   procedure Clear (Self : in out Instance);

   -- Add element to heap. This is done in O(log n) time where n is the current size of the heap.
   -- Return: True means add was successful. False means there is no more room in the heap.
   function Push (Self : in out Instance; Element : in Element_Type) return Boolean;

   -- Return top element (highest priority) from the heap. This is done in O(log n) where n is
   -- the current size of the heap.
   -- Return: True means element was found. False means the heap is empty
   function Pop (Self : in out Instance; Element : out Element_Type) return Boolean;

   -- Return top element (highest priority) from the heap. This is done in O(1) time.
   -- Return: True means element was found. False means the heap is empty
   function Peek (Self : in Instance; Element : out Element_Type) return Boolean;

   -- Get current number of elements stored in heap:
   function Get_Size (Self : in Instance) return Natural
      with Inline => True;
      -- Get the maximum number of elements that the heap has ever stored since init, "high water mark":
   function Get_Maximum_Size (Self : in Instance) return Natural
      with Inline => True;
      -- Get the maximum number of elements that the heap can store before becoming full.
   function Get_Capacity (Self : in Instance) return Positive
      with Inline => True;

      -- Full and empty boolean functions:
   function Is_Full (Self : in Instance) return Boolean
      with Inline => True;
   function Is_Empty (Self : in Instance) return Boolean
      with Inline => True;

private

   -- A node that makes up the heap. We include an order along side the element to
   -- aid in ensuring the stability of the heap.
   type Node is record
      Order : Interfaces.Unsigned_32; -- Used to enforce stability
      Element : Element_Type;
   end record;

   -- A node tree type, just an array of nodes.
   type Node_Array is array (Natural range <>) of Node;
   type Node_Array_Access is access Node_Array;

   -- The heap record definition:
   type Instance is tagged limited record
      Order : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'First;   -- Used to keep heap stable
      Count : Natural := Natural'First;       -- Current number of elements on heap
      Max_Count : Natural := Natural'First;
      Tree : Node_Array_Access := null;       -- The heap data itself
   end record;

end Heap;
