with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;
with Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Static.Assertion;
with Simple_Variable.Assertion;

package body Global is
   protected body Action is
      procedure Get (A : out Action_T) is
      begin
         A := Act;
         Act := Nothing;
      end Get;
      procedure Set (A : in Action_T) is
      begin
         Act := A;
      end Set;
   end Action;

   task body Unblock is
      Act : Action_T;
      Ignore : Natural;
      One_Second : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (200);
      One_Second_Later : Ada.Real_Time.Time;
      Len : Natural;
      Static_Var : Static.T := (others => 0);
      Variable_Var : Simple_Variable.T := (Length => 0, Buffer => [others => 0]);
      Bytes : Basic_Types.Byte_Array (0 .. 100);
      Pri_Data : Data;
      use Ada.Real_Time;
      use Simple_Variable;
      use Static.Assertion;
      use Simple_Variable.Assertion;
   begin
      loop
         One_Second_Later := Ada.Real_Time.Clock + One_Second;
         The_Action.Get (Act);
         case Act is
            when Pop =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler removing bytes item from queue.");
               Peek_Assert.Eq (Queue.Peek_Length (Priority => Pri_Data, Length => Len), Success);
               pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
               Natural_Assert.Eq (Len, 10);
               Pop_Assert.Eq (Queue.Pop (Priority => Pri_Data, Bytes => Bytes, Length => Len), Success);
               pragma Assert (Pri_Data = (Pri => 1, Data_Type => Bytes_Type));
               Natural_Assert.Eq (Len, 9);
               Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [2, 3, 4, 5, 6, 7, 8, 9, 10]);
               Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
            when Pop_Static =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler removing static item from queue.");
               Peek_Assert.Eq (Queue.Peek_Length (Priority => Pri_Data, Length => Len), Success);
               pragma Assert (Pri_Data = (Pri => 2, Data_Type => Static_Type));
               Natural_Assert.Eq (Len, 4);
               Static_Var := (others => 0);
               Pop_Type_Assert.Eq (Pop_Static (Queue, Priority => Pri_Data, Dest => Static_Var), Success);
               pragma Assert (Pri_Data = (Pri => 2, Data_Type => Static_Type));
               Static_Assert.Eq (Static_Var, (One => 15, Two => 13, Three => 12));
               Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
            when Pop_Variable =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler removing variable item from queue.");
               Peek_Assert.Eq (Queue.Peek_Length (Priority => Pri_Data, Length => Len), Success);
               pragma Assert (Pri_Data = (Pri => 3, Data_Type => Variable_Type));
               Put_Line ("len: " & Natural'Image (Len));
               Natural_Assert.Eq (Len, 7);
               Variable_Var := (0, [others => 0]);
               Pop_Type_Assert.Eq (Pop_Simple_Variable (Queue, Priority => Pri_Data, Dest => Variable_Var), Success);
               pragma Assert (Pri_Data = (Pri => 3, Data_Type => Variable_Type));
               Simple_Variable_Assert.Eq (Variable_Var, (6, [10, 9, 8, 7, 6, 5, others => 0]));
               Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
            when Push =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler adding item to queue.");
               Push_Assert.Eq (Queue.Push ((Pri => 1, Data_Type => Bytes_Type), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]), Success);
               Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
            when Push_Static =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler adding static item to queue.");
               Push_Assert.Eq (Push_Static (Queue, (Pri => 2, Data_Type => Static_Type), (One => 7, Two => 8, Three => 9)), Success);
               Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
            when Push_Variable =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler adding variable item to queue.");
               Push_Variable_Length_Type_Assert.Eq (Push_Simple_Variable (Queue, (Pri => 3, Data_Type => Variable_Type), (3, [99, 99, 99, 99, others => 255])), Success);
               Put_Line (Natural'Image (Queue.Num_Bytes_Free) & " bytes free in queue.");
            when Pop_Error =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler popping from a queue where another task is already blocked.");
               Pop_Block_Assert.Eq (Queue.Pop_Block (Priority => Pri_Data, Bytes => Bytes, Length => Len), Error);
               Pop_Type_Block_Assert.Eq (Pop_Static_Block (Queue, Priority => Pri_Data, Dest => Static_Var), Error);
               Pop_Type_Block_Assert.Eq (Pop_Simple_Variable_Block (Queue, Priority => Pri_Data, Dest => Variable_Var), Error);
               Peek_Block_Assert.Eq (Queue.Peek_Length_Block (Priority => Pri_Data, Length => Len), Error);
               Put_Line ("Error received, now pushing to unblock other task.");
               Push_Assert.Eq (Queue.Push ((Pri => 1, Data_Type => Bytes_Type), [6, 6, 6, 7, 7, 8]), Success);
            when Push_Error =>
               The_Action.Set (Nothing);
               delay until One_Second_Later;
               Put_Line ("Handler pushing from a queue where another task is already blocked.");
               Push_Block_Assert.Eq (Queue.Push_Block ((Pri => 1, Data_Type => Bytes_Type), [4, 4, 4, 4, 4, 5, 5, 5, 5, 5]), Error);
               Push_Block_Assert.Eq (Push_Static_Block (Queue, (Pri => 1, Data_Type => Static_Type), (One => 7, Two => 8, Three => 9)), Error);
               Push_Variable_Length_Type_Block_Assert.Eq (Push_Simple_Variable_Block (Queue, (Pri => 1, Data_Type => Variable_Type), (3, [99, 99, 99, 99, others => 255])), Error);
               Put_Line ("Error received, now popping to unblock other task.");
               Pop_Assert.Eq (Queue.Pop (Priority => Pri_Data, Bytes => Bytes, Length => Len), Success);
            when Quit =>
               Put_Line ("Exiting unblock thread.");
               exit;
            when Nothing =>
               null;
         end case;
      end loop;
   end Unblock;
end Global;
