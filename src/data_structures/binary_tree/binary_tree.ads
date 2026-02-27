--
-- Generic binary tree package which has O(n) insertion/removal time and
-- O(log n) search time.
--
-- This binary tree package is optimized to have the fastest look
-- up time possible. It uses a sorted array of elements under the
-- hood so lookups only require integer related math to determine
-- the next index to look at, as opposed to actually traversing
-- the elements of a tree. As a result, insertions take a while because
-- the tree must remain sorted. Insertions may require a lot of
-- copies, moving elements around in the underlying array to ensure
-- the sorted property. As such, this tree is best used when insertions
-- only need to occur once at program startup, and during the rest of
-- the program execution, only look ups are used.
--
-- Removal of elements is done in a similar way to insertion, and takes
-- O(n) time.
--
-- If you need fast insertion/removal time into the binary tree, you should
-- not use this binary tree package, as these operations could get cripplingly
-- slow as the value of N gets very large.
--
-- To instantiate the generic binary tree you must define the type as
-- well as two functions the compare "less than" and "greater than" of
-- the type and return a boolean stating whether the condition is
-- True or False.
--
generic
   type Element_Type is private;
   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   with function ">" (Left, Right : Element_Type) return Boolean is <>;
package Binary_Tree is
   type Instance is tagged limited private;
   type Instance_Access is access all Instance;

   procedure Init (Self : in out Instance; Maximum_Size : in Positive);
   procedure Destroy (Self : in out Instance);

   -- Add element to tree. This is done in O(n) time where n is the current size of the tree.
   -- Return: True means add was successful. False means there is no more room in the tree.
   function Add (Self : in out Instance; Element : in Element_Type) return Boolean;
   -- Remove element from tree. This is done in O(n) time where n is the current size of the tree.
   -- Return: True means remove was successful. False means the provided index is not found in the tree.
   function Remove (Self : in out Instance; Element_Index : in Positive) return Boolean;
   -- Search for element in tree. This is done in O(log n) where n is the current size of the tree.
   -- Return: True means element was found. False means it was not. The element and index in the array where it was found are also returned.
   function Search (Self : in Instance; Element : in Element_Type; Element_Found : out Element_Type; Element_Index : out Positive) return Boolean;
   -- Get an element via its index. This can be helpful to quickly retrieve an element in O(1) time if you have already obtained its index via "search".
   function Get (Self : in Instance; Element_Index : in Positive) return Element_Type;
   -- Set an element via its index. This can be helpful to quickly set an element in O(1) time if you have already obtained its index via "search".
   procedure Set (Self : in out Instance; Element_Index : in Positive; Element : in Element_Type);
   -- Clear the tree. This is done in O(1) time.
   procedure Clear (Self : in out Instance);
   -- Get functions:
   function Get_Size (Self : in Instance) return Natural;
   function Get_Capacity (Self : in Instance) return Positive;
   -- Functions to get the first and last index in the tree. If the tree is empty, then first returns 1 and last returns 0.
   function Get_First_Index (Self : in Instance) return Positive;
   function Get_Last_Index (Self : in Instance) return Natural;

private
   type Element_Array is array (Positive range <>) of Element_Type;
   type Element_Array_Access is access Element_Array;

   type Instance is tagged limited record
      Size : Natural := 0;
      -- A sorted list of elements that can be easily used for
      -- an O(log n) search.
      Tree : Element_Array_Access := null;
   end record;
end Binary_Tree;
