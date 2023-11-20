with Ada.Text_IO; use Ada.Text_IO;
with Global; use Global;
with Natural_Fifo;

procedure Test is
   use Natural_Fifo;
   Value : Natural;
   Ignore : Natural;
begin
   Put ("Initialize queue test... ");
   My_Queue.Init (Global.Queue_Length);
   pragma Assert (My_Queue.Get_Depth = Global.Queue_Length);
   Put_Line ("passed.");

   Put ("Empty queue test... ");
   pragma Assert (not My_Queue.Is_Full);
   pragma Assert (My_Queue.Is_Empty);
   Put_Line ("passed.");

   for I in 0 .. 1 loop
      Put ("Filling queue test... ");
      for Index in Natural'First .. Queue_Length - 1 loop
         pragma Assert (My_Queue.Push (Index) = Success);
         pragma Assert (My_Queue.Get_Count = Index + 1);
      end loop;
      pragma Assert (My_Queue.Push (16) = Full);
      pragma Assert (My_Queue.Is_Full);
      Put_Line ("passed.");

      Put ("Emptying queue test... ");
      for Index in Natural'First .. Queue_Length - 1 loop
         pragma Assert (My_Queue.Peek (Value) = Success);
         pragma Assert (Value = Index, Integer'Image (Value) & " = " & Integer'Image (Index));
         pragma Assert (My_Queue.Get_Count = Queue_Length - Index);
         pragma Assert (My_Queue.Pop (Value) = Success);
         pragma Assert (Value = Index, Integer'Image (Value) & " = " & Integer'Image (Index));
         pragma Assert (My_Queue.Get_Count = Queue_Length - Index - 1);
      end loop;
      pragma Assert (My_Queue.Peek (Ignore) = Empty);
      pragma Assert (My_Queue.Pop (Ignore) = Empty);
      pragma Assert (My_Queue.Is_Empty);
      Put_Line ("passed.");
   end loop;

   Put ("Destroy queue test... ");
   My_Queue.Destroy;
   Put_Line ("passed.");
end Test;
