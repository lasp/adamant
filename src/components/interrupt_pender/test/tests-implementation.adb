--------------------------------------------------------------------------------
-- Interrupt_Pender Tests Body
--------------------------------------------------------------------------------

-- Ada includes:
with Ada.Interrupts;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

-- System includes:
with Interrupt_Sender;
with Tick.Assertion; use Tick.Assertion;
with Tester_Interrupt_Handler;
with Safe_Deallocator;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Dynamically allocate the generic component tester:
      Self.Tester := new Component_Tester_Package.Instance (Tester_Interrupt_Handler.Handler'Access);

      -- Set the logger in the component
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);

      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      -- Free the tester component:
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Component_Tester_Package.Instance, Name => Component_Tester_Package.Instance_Access);
   begin
      -- Free component heap:
      Self.Tester.Final_Base;

      -- Delete tester:
      Free_If_Testing (Self.Tester);
   end Tear_Down_Test;

   Epoch : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   procedure Put_Line_With_Time (The_String : in String) is
      The_Duration : constant Duration := Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Epoch);
   begin
      Put_Line (Duration'Image (The_Duration) & " " & The_String);
      Flush;
   end Put_Line_With_Time;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Interrupt_Handling (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Interrupt_Id : Ada.Interrupts.Interrupt_ID renames Component_Tester_Package.Interrupt_Id;

      -- Function to sleep for a while. The test needs to yield the processor to the component task
      -- in order to test its execution. This is the easiest way to do that, since raising an
      -- interrupt does not automatically yield the CPU.
      procedure Sleep_A_Bit is
         Wait_Time : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (500_000);
         End_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Wait_Time;
      begin
         delay until End_Time;
      end Sleep_A_Bit;

      The_Tick : Tick.T;

      -- Task which sleeps for a bit and then sends an interrupt to the component.
      -- This allows us to simulate an asynchronous interrupt, which allows us to test
      -- blocking the test execution below on "Wait_On_Interrupt_Data_Type_Get".
      task Send_Interrupt_In_A_Bit;
      task body Send_Interrupt_In_A_Bit is
      begin
         Put_Line_With_Time ("Starting task...");
         Sleep_A_Bit;
         Sleep_A_Bit;
         Sleep_A_Bit;
         Put_Line_With_Time ("Sending interrupt from task...");
         Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      end Send_Interrupt_In_A_Bit;
   begin
      -- Generate an interrupts, and yield the cpu, and then make sure that the wait
      -- returns immediately.
      Put_Line_With_Time ("Sending Interrupt.");
      Interrupt_Sender.Generate_Interrupt (Interrupt_Id);
      Sleep_A_Bit;
      Put_Line_With_Time ("Waiting on Interrupt...");
      The_Tick := T.Wait_On_Interrupt_Data_Type_Get;
      Put_Line_With_Time ("Interrupt Received.");
      Tick_Assert.Eq (The_Tick, (T.System_Time, 1));

      -- Wait on the component for a bit, and then send an interrupt and make
      -- sure it releases.
      Put_Line_With_Time ("");
      Put_Line_With_Time ("Waiting on Interrupt from task...");
      The_Tick := T.Wait_On_Interrupt_Data_Type_Get;
      Put_Line_With_Time ("Interrupt Received from task.");
      Tick_Assert.Eq (The_Tick, (T.System_Time, 2));
   end Test_Interrupt_Handling;

end Tests.Implementation;
