-- Note: A certain test (see below) requires this to be enabled,
-- and the program run in GDB. For long term testing we won't
-- enable this, since it uses ravenscar the test actually never
-- terminates.
-- pragma Profile (Ravenscar);
with Ada.Text_IO; use Ada.Text_IO;
with Global; use Global;

procedure Test is
   use Global.Natural_Queue;
   Value : Natural;
   Queue_Length : constant Positive := 10;
begin
   Put ("Init queue test... ");
   My_Queue.Init (Queue_Length);
   pragma Assert (Queue_Length = My_Queue.Get_Depth);
   Put_Line ("passed.");

   Put ("Empty queue test... ");
   pragma Assert (My_Queue.Is_Full = False);
   pragma Assert (My_Queue.Is_Empty);
   Put_Line ("passed.");

   for I in 0 .. 1 loop
      Put_Line ("Filling queue test... ");
      for Index in Natural'First .. Queue_Length - 1 loop
         pragma Assert (My_Queue.Push (Index) = Success);
      end loop;
      pragma Assert (My_Queue.Is_Full);
      pragma Assert (My_Queue.Push (Queue_Length) = Full);
      -- Note: The following can only be tested in gdb with ravenscar
      -- enabled above, and in the global package. If you want to test
      -- this code, then comment out the '---' section below this one.
      -------------------------------------------------------------
      ---- Should cause harness to error and then continue on
      ---- unblocking us.
      --The_Action.Set (Push_Error);
      --pragma Assert (My_Queue.Push_Block (Queue_Length) = Success);
      -- -------------------------------------------------------------
      -----------------------------------------------------------
      -- Should block until test harness pops item.
      The_Action.Set (Pop);
      pragma Assert (My_Queue.Push_Block (Queue_Length) = Success);
      -----------------------------------------------------------
      pragma Assert (My_Queue.Is_Full);
      pragma Assert (My_Queue.Get_Count = My_Queue.Get_Depth);
      pragma Assert (My_Queue.Get_Depth = My_Queue.Get_Max_Count);
      pragma Assert (My_Queue.Get_Depth = Queue_Length);
      Put_Line ("passed.");

      Put_Line ("Emptying queue test... ");
      for Index in Natural'First .. Queue_Length - 1 loop
         pragma Assert (My_Queue.Peek (Value) = Success);
         pragma Assert (Value = Index + 1, Integer'Image (Value) & " = " & Integer'Image (Index));
         pragma Assert (My_Queue.Pop (Value) = Success);
         pragma Assert (Value = Index + 1, Integer'Image (Value) & " = " & Integer'Image (Index));
      end loop;
      pragma Assert (My_Queue.Is_Empty);
      pragma Assert (My_Queue.Pop (Value) = Empty);
      pragma Assert (My_Queue.Peek (Value) = Empty);
      -- Note: The following can only be tested in gdb with ravenscar
      -- enabled above, and in the global package. If you want to test
      -- this code, then comment out the '---' section below this one.
      -- -----------------------------------------------------------
      -- -- Should cause harness to error and then continue on
      -- -- unblocking us.
      -- The_Action.Set (Pop_Error);
      -- pragma Assert (My_Queue.Peek_Block (Value) = Success);
      -- -----------------------------------------------------------
      -----------------------------------------------------------
      -- Should block until test harness pushes item.
      The_Action.Set (Push);
      pragma Assert (My_Queue.Peek_Block (Value) = Success);
      -----------------------------------------------------------
      pragma Assert (Value = 17);
      pragma Assert (not My_Queue.Is_Empty);
      pragma Assert (My_Queue.Pop_Block (Value) = Success);
      pragma Assert (Value = 17);
      pragma Assert (My_Queue.Is_Empty);
      pragma Assert (My_Queue.Get_Count = 0);
      pragma Assert (My_Queue.Get_Depth = My_Queue.Get_Max_Count);
      pragma Assert (My_Queue.Get_Depth = Queue_Length);
      Put_Line ("passed.");
   end loop;

   The_Action.Set (Quit);

   Put ("Destroy queue test... ");
   My_Queue.Destroy;
   Put_Line ("passed.");
end Test;
