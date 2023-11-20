with Protected_Priority_Queue;
with Smart_Assert;
with Static;
with Simple_Variable;

package Global is

   type Element_Type is (Bytes_Type, Static_Type, Variable_Type); -- Use label to track the queue element's type

   -- Define the data type to store on the queue:
   type Data is record
      Pri : Natural; -- priority
      Data_Type : Element_Type;
   end record;

   function Data_Greater_Than (A, B : in Data) return Boolean is (A.Pri > B.Pri);
   function Data_Equal_To (A, B : in Data) return Boolean is (A.Pri = B.Pri);

   -- Global (library level declarations):
   package Data_Priority_Queue is new Protected_Priority_Queue (Data, Data_Greater_Than, Data_Equal_To);
   use Data_Priority_Queue;
   Queue : Data_Priority_Queue.Instance;

   type Action_T is (Push, Push_Static, Push_Variable, Pop, Pop_Static, Pop_Variable, Pop_Error, Push_Error, Quit, Nothing);
   protected type Action is
      procedure Get (A : out Action_T);
      procedure Set (A : in Action_T);
   private
      Act : Action_T := Nothing;
   end Action;
   The_Action : Action;

   task Unblock;

   -- Status assertions:
   package Push_Assert is new Smart_Assert.Basic (Push_Status, Push_Status'Image);
   package Pop_Assert is new Smart_Assert.Basic (Pop_Status, Pop_Status'Image);
   package Peek_Assert is new Smart_Assert.Basic (Peek_Status, Peek_Status'Image);
   package Push_Block_Assert is new Smart_Assert.Basic (Push_Block_Status, Push_Block_Status'Image);
   package Pop_Block_Assert is new Smart_Assert.Basic (Pop_Block_Status, Pop_Block_Status'Image);
   package Peek_Block_Assert is new Smart_Assert.Basic (Peek_Block_Status, Peek_Block_Status'Image);
   package Push_Variable_Length_Type_Assert is new Smart_Assert.Basic (Push_Variable_Length_Type_Status, Push_Variable_Length_Type_Status'Image);
   package Push_Variable_Length_Type_Block_Assert is new Smart_Assert.Basic (Push_Variable_Length_Type_Block_Status, Push_Variable_Length_Type_Block_Status'Image);
   package Pop_Type_Assert is new Smart_Assert.Basic (Pop_Type_Status, Pop_Type_Status'Image);
   package Pop_Type_Block_Assert is new Smart_Assert.Basic (Pop_Type_Block_Status, Pop_Type_Block_Status'Image);

   -- Typed push/pop/peek functions for static:
   function Push_Static is new Push_Type (Static.T);
   function Push_Static_Block is new Push_Type_Block (Static.T);
   function Pop_Static is new Pop_Type (Static.T);
   function Pop_Static_Block is new Pop_Type_Block (Static.T);

   -- Typed push/pop/peek functions for variable:
   function Push_Simple_Variable is new Push_Variable_Length_Type (Simple_Variable.T, Simple_Variable.Serialized_Length);
   function Push_Simple_Variable_Block is new Push_Variable_Length_Type_Block (Simple_Variable.T, Simple_Variable.Serialized_Length);
   function Pop_Simple_Variable is new Pop_Variable_Length_Type (Simple_Variable.T, Simple_Variable.Serialized_Length);
   function Pop_Simple_Variable_Block is new Pop_Variable_Length_Type_Block (Simple_Variable.T, Simple_Variable.Serialized_Length);
end Global;
