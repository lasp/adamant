with Ada.Text_IO; use Ada.Text_IO;
with Global; use Global;
with Basic_Assertions; use Basic_Assertions;
with Basic_Types;
with Static;
with Simple_Variable;
with Static.Assertion; use Static.Assertion;
with Simple_Variable.Assertion; use Simple_Variable.Assertion;
with Ada.Real_Time;

procedure Test is
   use Global.Data_Priority_Queue;

   -- Other vars:
   Len : Natural;
   Static_Var : Static.T := (others => 0);
   Variable_Var : constant Simple_Variable.T := (Length => 6, Buffer => [10, 9, 8, 7, 6, 5, others => 255]);
   Variable_Var2 : Simple_Variable.T := (Length => 0, Buffer => [others => 0]);
   Queue_Size : constant Positive := Simple_Variable.Size_In_Bytes * 3;
   Bytes : Basic_Types.Byte_Array (0 .. 100);
   Pri_Data : Data;
   Ignore : Data;
begin
   Put_Line ("Init queue test... ");
   Queue.Init (Element_Size => Simple_Variable.Size_In_Bytes, Depth => 3);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Empty queue test... ");
   Natural_Assert.Eq (Queue.Num_Bytes_Free, Queue_Size);
   Natural_Assert.Eq (Queue.Num_Bytes_Used, 0);
   Natural_Assert.Eq (Queue.Max_Num_Bytes_Used, 0);
   Natural_Assert.Eq (Queue.Size_In_Bytes, Queue_Size);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 0);
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Empty);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Filling queue test... ");
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Push_Assert.Eq (Queue.Push ((Pri => 1, Data_Type => Bytes_Type), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 1);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Push a static type:
   Push_Assert.Eq (Push_Static (Queue, (Pri => 2, Data_Type => Static_Type), (One => 15, Two => 13, Three => 12)), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 2);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 2);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Pri => 0, Data_Type => Variable_Type), (Length => 21, Buffer => [others => 233])), Serialization_Failure);
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Pri => 3, Data_Type => Variable_Type), Variable_Var), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking push test... ");
   -- Push some bytes:
   Push_Assert.Eq (Queue.Push ((Pri => 100, Data_Type => Bytes_Type), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Full);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Pop_Variable);
   Push_Block_Assert.Eq (Queue.Push_Block ((Pri => 0, Data_Type => Bytes_Type), [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), Success);
   Put_Line ("pushed item.");
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Push a static type:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Success);
   pragma Assert (Pri_Data = (Pri => 2, Data_Type => Static_Type));
   Natural_Assert.Eq (Len, 4);
   Push_Assert.Eq (Push_Static (Queue, (Pri => 99, Data_Type => Static_Type), (One => 19, Two => 3, Three => 2)), Full);
   Natural_Assert.Eq (Len, 4);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Pop_Static);
   Push_Block_Assert.Eq (Push_Static_Block (Queue, (Pri => 2, Data_Type => Static_Type), (One => 15, Two => 13, Three => 12)), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Pri => 99, Data_Type => Variable_Type), Variable_Var), Full);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Pop_Static);
   Push_Variable_Length_Type_Block_Assert.Eq (Push_Simple_Variable_Block (Queue, (Pri => 3, Data_Type => Variable_Type), (9, [8, 8, 8, 8, 8, 8, 8, 8, 8, others => 255])), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Empty queue test... ");
   -- Pop variable type:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Success);
   pragma Assert (Pri_Data = (Pri => 3, Data_Type => Variable_Type));
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Pop_Type_Assert.Eq (Pop_Simple_Variable (Queue, Pri_Data, Variable_Var2), Success);
   pragma Assert (Pri_Data = (Pri => 3, Data_Type => Variable_Type));
   Simple_Variable_Assert.Eq (Variable_Var2, (9, [8, 8, 8, 8, 8, 8, 8, 8, 8, others => 0]));
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 2);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Pop bytes:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Success);
   pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
   Natural_Assert.Eq (Len, 10);
   Pop_Assert.Eq (Queue.Pop (Pri_Data, Bytes, Len), Success);
   pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Pop bytes:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Success);
   pragma Assert (Pri_Data = (Pri => 0, Data_Type => Bytes_Type));
   Natural_Assert.Eq (Len, 10);
   Pop_Assert.Eq (Queue.Pop (Pri_Data, Bytes, Len), Success);
   pragma Assert (Pri_Data = (Pri => 0, Data_Type => Bytes_Type));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking pop test... ");
   -- Pop bytes:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Empty);
   Pop_Assert.Eq (Queue.Pop (Pri_Data, Bytes, Length => Len), Empty);
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Pop_Block_Assert.Eq (Queue.Pop_Block (Pri_Data, Bytes, Length => Len), Success);
   Put_Line ("item popped.");
   pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Pop_Block_Assert.Eq (Queue.Pop_Block (Pri_Data, Bytes, Length => Len), Success);
   pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Pop bytes and length:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Empty);
   Pop_Assert.Eq (Queue.Pop (Pri_Data, Bytes, Length => Len), Empty);
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Pop_Block_Assert.Eq (Queue.Pop_Block (Pri_Data, Bytes, Length => Len), Success);
   pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Push);
   Peek_Block_Assert.Eq (Queue.Peek_Length_Block (Pri_Data, Length => Len), Success);
   pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
   Put_Line ("item peeked.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on nonempty queue... ");
   Pop_Block_Assert.Eq (Queue.Pop_Block (Pri_Data, Bytes, Length => Len), Success);
   pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
   Put_Line ("item popped.");
   Put_Line ("len: " & Natural'Image (Len));
   Natural_Assert.Eq (Len, 10);
   Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]);
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Pop static:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Empty);
   Pop_Type_Assert.Eq (Pop_Static (Queue, Pri_Data, Static_Var), Empty);
   Put_Line ("Waiting on empty queue (static)... ");
   The_Action.Set (Push_Static);
   Pop_Type_Block_Assert.Eq (Pop_Static_Block (Queue, Pri_Data, Static_Var), Success);
   pragma Assert (Pri_Data = (Pri => 2, Data_Type => Static_Type));
   Put_Line ("item popped.");
   Static_Assert.Eq (Static_Var, (One => 7, Two => 8, Three => 9));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue (static)... ");
   The_Action.Set (Push_Static);
   Pop_Type_Block_Assert.Eq (Pop_Static_Block (Queue, Pri_Data, Static_Var), Success);
   pragma Assert (Pri_Data = (Pri => 2, Data_Type => Static_Type));
   Put_Line ("item popped.");
   Static_Assert.Eq (Static_Var, (One => 7, Two => 8, Three => 9));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Pop variable:
   Peek_Assert.Eq (Queue.Peek_Length (Ignore, Len), Empty);
   Pop_Type_Assert.Eq (Pop_Simple_Variable (Queue, Pri_Data, Variable_Var2), Empty);
   Put_Line ("Waiting on empty queue (variable)... ");
   The_Action.Set (Push_Variable);
   Pop_Type_Block_Assert.Eq (Pop_Simple_Variable_Block (Queue, Pri_Data, Variable_Var2), Success);
   pragma Assert (Pri_Data = (Pri => 3, Data_Type => Variable_Type));
   Put_Line ("item popped.");
   Simple_Variable_Assert.Eq (Variable_Var2, (3, [99, 99, 99, others => 0]));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on empty queue (variable)... ");
   The_Action.Set (Push_Variable);
   Peek_Block_Assert.Eq (Queue.Peek_Length_Block (Pri_Data, Len), Success);
   pragma Assert (Pri_Data = (Pri => 3, Data_Type => Variable_Type));
   Put_Line ("item peeked.");
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on nonempty queue (variable)... ");
   Pop_Type_Block_Assert.Eq (Pop_Simple_Variable_Block (Queue, Pri_Data, Variable_Var2), Success);
   pragma Assert (Pri_Data = (Pri => 3, Data_Type => Variable_Type));
   Put_Line ("item popped.");
   Simple_Variable_Assert.Eq (Variable_Var2, (3, [99, 99, 99, others => 0]));
   Natural_Assert.Eq (Queue.Num_Elements, 0);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking pop error test... ");
   -- Pop bytes:
   Peek_Assert.Eq (Queue.Peek_Length (Pri_Data, Len), Empty);
   Pop_Assert.Eq (Queue.Pop (Pri_Data, Bytes, Length => Len), Empty);
   Put_Line ("Waiting on empty queue... ");
   The_Action.Set (Pop_Error);
   Pop_Block_Assert.Eq (Queue.Pop_Block (Pri_Data, Bytes, Length => Len), Success);
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
   Push_Assert.Eq (Queue.Push ((Pri => 11, Data_Type => Bytes_Type), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 1);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Push a static type:
   Push_Assert.Eq (Push_Static (Queue, (Pri => 11, Data_Type => Static_Type), (One => 15, Two => 13, Three => 12)), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 2);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Pri => 0, Data_Type => Static_Type), (Length => 21, Buffer => [others => 233])), Serialization_Failure);
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Pri => 12, Data_Type => Static_Type), Variable_Var), Success);
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Blocking push error test... ");
   -- Push some bytes:
   Push_Assert.Eq (Queue.Push ((Pri => 11, Data_Type => Bytes_Type), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Full);
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Push_Error);
   Push_Block_Assert.Eq (Queue.Push_Block ((Pri => 12, Data_Type => Bytes_Type), [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]), Success);
   Put_Line ("pushed item.");
   Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Push a static type:
   Push_Assert.Eq (Push_Static (Queue, (Pri => 11, Data_Type => Static_Type), (One => 5, Two => 3, Three => 2)), Full);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Push_Error);
   Push_Block_Assert.Eq (Push_Static_Block (Queue, (Pri => 11, Data_Type => Static_Type), (One => 5, Two => 3, Three => 2)), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
   -- Push a variable type:
   Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Pri => 11, Data_Type => Variable_Type), Variable_Var), Full);
   Put_Line ("Waiting on full queue... ");
   The_Action.Set (Push_Error);
   Push_Variable_Length_Type_Block_Assert.Eq (Push_Simple_Variable_Block (Queue, (Pri => 11, Data_Type => Variable_Type), (9, [8, 8, 8, 8, 8, 8, 8, 8, 8, others => 255])), Success);
   Put_Line ("pushed item.");
   Natural_Assert.Eq (Queue.Num_Elements, 3);
   Natural_Assert.Eq (Queue.Max_Num_Elements, 3);
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
