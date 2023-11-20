with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;

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
      use Natural_Queue;
      Act : Action_T;
      Ignore : Natural;
      One_Second : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1_000);
      One_Second_Later : Ada.Real_Time.Time;
      use Ada.Real_Time;
   begin
      loop
         One_Second_Later := Ada.Real_Time.Clock + One_Second;
         The_Action.Get (Act);
         case Act is
            when Pop =>
               delay until One_Second_Later;
               Put_Line ("Handler removing item from queue.");
               pragma Assert (My_Queue.Pop (Ignore) = Success);
            when Push =>
               delay until One_Second_Later;
               Put_Line ("Handler adding item to queue.");
               pragma Assert (My_Queue.Push (17) = Success);
            when Pop_Error =>
               delay until One_Second_Later;
               Put_Line ("Handler popping from a queue where another task is already blocked.");
               pragma Assert (My_Queue.Pop_Block (Ignore) = Error);
               Put_Line ("Error received, now pushing to unblock other task.");
               pragma Assert (My_Queue.Push (17) = Success);
            when Push_Error =>
               delay until One_Second_Later;
               Put_Line ("Handler pushing from a queue where another task is already blocked.");
               pragma Assert (My_Queue.Push_Block (17) = Error);
               Put_Line ("Error received, now popping to unblock other task.");
               pragma Assert (My_Queue.Pop (Ignore) = Success);
            when Quit =>
               exit;
            when Nothing =>
               null;
         end case;
      end loop;
   end Unblock;
end Global;
