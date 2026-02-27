with Ada.Text_IO; use Ada.Text_IO;
with Global; use Global;
with Basic_Assertions; use Basic_Assertions;
with Variable_Queue;
with Basic_Types.Representation;
with Static;
with Simple_Variable;
with Static.Assertion; use Static.Assertion;
with Simple_Variable.Assertion; use Simple_Variable.Assertion;
with Simple_Variable.Representation;
with Ada.Real_Time;

procedure Test is
   use Variable_Queue;

   -- Other vars:
   Len : Natural;
   Static_Var : Static.T := (others => 0);
   Ignore_Static_Var : Static.T := (others => 0);
   Variable_Var : constant Simple_Variable.T := (Length => 6, Buffer => [10, 9, 8, 7, 6, 5, others => 255]);
   Variable_Var2 : Simple_Variable.T := (Length => 0, Buffer => [others => 0]);
   Ignore_Variable_Var2 : Simple_Variable.T := (Length => 0, Buffer => [others => 0]);
   Queue_Size : constant Positive := 40;
   Queue_Bytes : aliased Basic_Types.Byte_Array := [0 .. Queue_Size - 1 => 0];
   Bytes : Basic_Types.Byte_Array (0 .. 100);
begin
   Put_Line ("Init queue test... ");
   Queue.Init (Queue_Bytes'Unchecked_Access);
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Empty queue test... ");
   Natural_Assert.Eq (Queue.Num_Bytes_Free, Queue_Size);
   Natural_Assert.Eq (Queue.Num_Bytes_Used, 0);
   Natural_Assert.Eq (Queue.Max_Num_Bytes_Used, 0);
   Natural_Assert.Eq (Queue.Size_In_Bytes, Queue_Size);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 0);
   Pop_Assert.Eq (Queue.Peek_Length (Len), Empty);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Filling queue test... ");
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Push_Assert.Eq (Queue.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 1);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a static type:
   Push_Assert.Eq (Push_Static (Queue, (One => 15, Two => 13, Three => 12)), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 2);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 2);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Length => 21, Buffer => [others => 233])), Serialization_Failure);
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, Variable_Var), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking push test... ");
   -- Push some bytes:
   Push_Assert.Eq (Queue.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Too_Full);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Pop);
   Push_Block_Assert.Eq (Queue.Push_Block ([11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), Success);
   Put_Line ("pushed item.");
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a static type:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Success);
   Natural_Assert.Eq (Len, 4);
   Pop_Type_Assert.Eq (Peek_Static (Queue, Static_Var, Offset => 0), Success);
   Static_Assert.Eq (Static_Var, (One => 15, Two => 13, Three => 12));
   Push_Assert.Eq (Push_Static (Queue, (One => 5, Two => 3, Three => 2)), Too_Full);
   Natural_Assert.Eq (Len, 4);
   Pop_Type_Assert.Eq (Peek_Static (Queue, Static_Var, Offset => 0), Success);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Pop_Static);
   Push_Block_Assert.Eq (Push_Static_Block (Queue, (One => 5, Two => 3, Three => 2)), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, Variable_Var), Too_Full);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Pop_Variable);
   Push_Variable_Length_Type_Block_Assert.Eq (Push_Simple_Variable_Block (Queue, (9, [8, 8, 8, 8, 8, 8, 8, 8, 8, others => 255])), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Empty queue test... ");
   -- Pop bytes:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Success);
   Natural_Assert.Eq (Len, 10);
   Pop_Assert.Eq (Queue.Peek (Bytes, Len, Offset => 1), Success);
   Natural_Assert.Eq (Len, 9);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [12, 13, 14, 15, 16, 17, 18, 19, 20]);
   Pop_Assert.Eq (Queue.Pop (Bytes, Len, Offset => 3), Success);
   Natural_Assert.Eq (Len, 7);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [14, 15, 16, 17, 18, 19, 20]);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 2);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Pop static type:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Success);
   Natural_Assert.Eq (Len, 4);
   Pop_Type_Assert.Eq (Peek_Static (Queue, Ignore_Static_Var, Offset => 1), Deserialization_Failure);
   Pop_Type_Assert.Eq (Peek_Static (Queue, Static_Var, Offset => 0), Success);
   Static_Assert.Eq (Static_Var, (One => 5, Two => 3, Three => 2));
   Static_Var := (others => 0);
   Pop_Type_Assert.Eq (Pop_Static (Queue, Static_Var, Offset => 0), Success);
   Static_Assert.Eq (Static_Var, (One => 5, Two => 3, Three => 2));
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Pop variable type:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Success);
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   -- An offset here wouldn't usually work, but in this case the values of the array make it a success.
   Pop_Type_Assert.Eq (Peek_Simple_Variable (Queue, Variable_Var2, Offset => 1), Success);
   Put_Line ("Peeked: " & Simple_Variable.Representation.Image (Variable_Var2));
   Simple_Variable_Assert.Eq (Variable_Var2, (8, [8, 8, 8, 8, 8, 8, 8, 8, others => 0]));
   Pop_Type_Assert.Eq (Pop_Simple_Variable (Queue, Variable_Var2, Offset => 0), Success);
   Simple_Variable_Assert.Eq (Variable_Var2, (9, [8, 8, 8, 8, 8, 8, 8, 8, 8, others => 0]));
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking pop test... ");
   -- Pop bytes:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Empty);
   Pop_Assert.Eq (Queue.Peek (Bytes, Length => Len), Empty);
   Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Empty);
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Pop_Block_Assert.Eq (Queue.Pop_Block (Bytes, Length => Len), Success);
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Pop_Block_Assert.Eq (Queue.Peek_Block (Bytes, Length => Len), Success);
   Put_Line ("item peeked.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on nonempty queue... ");
   Pop_Block_Assert.Eq (Queue.Pop_Block (Bytes, Length => Len), Success);
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Pop bytes and length:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Empty);
   Pop_Assert.Eq (Queue.Peek (Bytes, Length => Len), Empty);
   Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Empty);
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Pop_Block_Assert.Eq (Queue.Pop_Block (Bytes, Length => Len), Success);
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Pop_Block_Assert.Eq (Queue.Peek_Length_Block (Length => Len), Success);
   Put_Line ("item peeked.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on nonempty queue... ");
   Pop_Block_Assert.Eq (Queue.Pop_Block (Bytes, Length => Len), Success);
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Pop static:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Empty);
   Pop_Type_Assert.Eq (Pop_Static (Queue, Ignore_Static_Var, Offset => 0), Empty);
   Pop_Type_Assert.Eq (Peek_Static (Queue, Static_Var, Offset => 0), Empty);
   Put_Line ("Waiting on empty queue (static)... ");
   The_Action.Set (Push_Static);
   Pop_Type_Block_Assert.Eq (Pop_Static_Block (Queue, Static_Var), Success);
   Put_Line ("item popped.");
   Static_Assert.Eq (Static_Var, (One => 7, Two => 8, Three => 9));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue (static)... ");
   The_Action.Set (Push_Static);
   Pop_Type_Block_Assert.Eq (Peek_Static_Block (Queue, Static_Var), Success);
   Put_Line ("item popped.");
   Static_Assert.Eq (Static_Var, (One => 7, Two => 8, Three => 9));
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on nonempty queue (static)... ");
   Pop_Type_Block_Assert.Eq (Pop_Static_Block (Queue, Static_Var), Success);
   Put_Line ("item popped.");
   Static_Assert.Eq (Static_Var, (One => 7, Two => 8, Three => 9));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Pop variable:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Empty);
   Pop_Type_Assert.Eq (Pop_Simple_Variable (Queue, Ignore_Variable_Var2, Offset => 0), Empty);
   Pop_Type_Assert.Eq (Peek_Simple_Variable (Queue, Ignore_Variable_Var2, Offset => 0), Empty);
   Put_Line ("Waiting on empty queue (variable)... ");
   The_Action.Set (Push_Variable);
   Pop_Type_Block_Assert.Eq (Pop_Simple_Variable_Block (Queue, Variable_Var2), Success);
   Put_Line ("item popped.");
   Simple_Variable_Assert.Eq (Variable_Var2, (3, [99, 99, 99, others => 0]));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue (variable)... ");
   The_Action.Set (Push_Variable);
   Pop_Type_Block_Assert.Eq (Peek_Simple_Variable_Block (Queue, Variable_Var2), Success);
   Put_Line ("item popped.");
   Simple_Variable_Assert.Eq (Variable_Var2, (3, [99, 99, 99, others => 0]));
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on nonempty queue (variable)... ");
   Pop_Type_Block_Assert.Eq (Pop_Simple_Variable_Block (Queue, Variable_Var2), Success);
   Put_Line ("item popped.");
   Simple_Variable_Assert.Eq (Variable_Var2, (3, [99, 99, 99, others => 0]));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking pop error test... ");
   -- Pop bytes:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Empty);
   Pop_Assert.Eq (Queue.Peek (Bytes, Length => Len), Empty);
   Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Empty);
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Pop_Error);
   Pop_Block_Assert.Eq (Queue.Pop_Block (Bytes, Length => Len), Success);
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 6);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [6, 6, 6, 7, 7, 8]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Filling queue test 2... ");
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Push_Assert.Eq (Queue.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a static type:
   Push_Assert.Eq (Push_Static (Queue, (One => 15, Two => 13, Three => 12)), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 2);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Length => 21, Buffer => [others => 233])), Serialization_Failure);
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, Variable_Var), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking push error test... ");
   -- Push some bytes:
   Push_Assert.Eq (Queue.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Too_Full);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Push_Error);
   Push_Block_Assert.Eq (Queue.Push_Block ([11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), Success);
   Put_Line ("pushed item.");
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a static type:
   Pop_Assert.Eq (Queue.Peek_Length (Len), Success);
   Natural_Assert.Eq (Len, 4);
   Pop_Type_Assert.Eq (Peek_Static (Queue, Static_Var, Offset => 0), Success);
   Static_Assert.Eq (Static_Var, (One => 15, Two => 13, Three => 12));
   Push_Assert.Eq (Push_Static (Queue, (One => 5, Two => 3, Three => 2)), Too_Full);
   Natural_Assert.Eq (Len, 4);
   Pop_Type_Assert.Eq (Peek_Static (Queue, Static_Var, Offset => 0), Success);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Push_Error);
   Push_Block_Assert.Eq (Push_Static_Block (Queue, (One => 5, Two => 3, Three => 2)), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, Variable_Var), Too_Full);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Push_Error);
   Push_Variable_Length_Type_Block_Assert.Eq (Push_Simple_Variable_Block (Queue, (9, [8, 8, 8, 8, 8, 8, 8, 8, 8, others => 255])), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Basic_Types.Representation.Image (Queue_Bytes));
   Put_Line ("passed.");
   Put_Line ("");

   The_Action.Set (Quit);

   -- Give time for other thread to exit before destroying the queue:
   declare
      use Ada.Real_Time;
      Three_Second : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (3_000);
      One_Second_Later : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Three_Second;
   begin
      delay until One_Second_Later;
   end;

   Put ("Destroy queue test... ");
   Queue.Destroy;
   Put_Line ("passed.");
end Test;
